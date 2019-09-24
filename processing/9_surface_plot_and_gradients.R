rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(grid)
library(foreach)
library(doMC)
registerDoMC(cores=7)

#load posteriors
load("data/derived/unnormalised_posteriors.RData")

#Set theme for plot
theme.goal <- theme(strip.text.x = element_text(size=12,face="bold",colour="black",family="Times"),
                    strip.text.y = element_text(size=12, face="bold","black",family="Times"),
                    strip.background = element_rect(colour="black", fill="gray"),
                    plot.background = element_rect(fill = "transparent"),
                    #plot.border= element_rect(fill = "transparent"),
                    panel.border = element_rect(color="black", linetype="solid",fill=NA),
                    panel.background = element_rect(fill = "transparent"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.x = element_text(size=12,colour="black",family="Times"),
                    axis.text.y = element_text(size=12,colour="black",family="Times"),
                    axis.title.x = element_text(size=18,colour="black",face="bold",family="Times",vjust=-0.5),
                    axis.title.y = element_text(size=18,colour="black",face="bold",family="Times",vjust=1),
                    #axis.ticks.length = unit(-0.2,"cm"),
                    legend.title=element_text(size=16,face="bold",family="Times"),
                    legend.text=element_text(size=14,family="Times"),
                    legend.key=element_rect(color='transparent',fill='white' ),
                    legend.title.align = 0.5,
                    legend.text.align = 0.5,
                    plot.title = element_text(size=24,face="bold",family="Times",hjust=0.5))

### FUNCTIONS ###

#Emulate ggplot colour palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



#calculate normalised parameters
posts_norm_tmp = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  filter(source=="obs")


#create four different sets of plots
#1) across experiments
#2) experiment 1
#3) experiment 2
#4) experiment 3

for(i in 1:4){


  if(i == 1){
    posts_norm = posts_norm_tmp %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s)
    label = ""
    print("Combined")
  }
  if(i == 2){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w2)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s)
    label = "_expt1"
    print("Experiment 1")
  }
  if(i == 3){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w1)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s)
    label = "_expt2"
    print("Experiment 2")
  }
  if(i == 4){
    posts_norm = posts_norm_tmp %>%
      filter(!is.na(w1) & !is.na(w2)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s)
    label = "_expt3"
    print("Experiment 3")
  }

### MEAN SURFACES ###
#
# generate data for different combinations of D and T
sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  ap = NA,
                  av = NA)

posts_norm = posts_norm %>%
  mutate(max = 2*(1-alpha)*sqrt(alpha/(1-alpha)))

#loop through different points in the surface, because it requires too much memory to calculate mean for all points simultaneously
surface_data <- foreach(j = 1:nrow(sim),.combine='rbind') %dopar% {

  sg = (posts_norm$delta>=0)*posts_norm$w1*sim$d[j]^posts_norm$delta + (posts_norm$delta<0)*posts_norm$w1*(1-sim$d[j]^-posts_norm$delta)
  tg = (posts_norm$tau>=0)*posts_norm$w2*sim$t[j]^posts_norm$tau + (posts_norm$tau<0)*posts_norm$w2*(1-sim$t[j]^-posts_norm$tau)
  stg = posts_norm$w3*posts_norm$max / (posts_norm$alpha*sim$t[j]/sim$d[j] + (1-posts_norm$alpha)*sim$d[j]/sim$t[j] )
  posts_norm$g = replace_na(sg,0) + replace_na(tg,0) + stg

  means = posts_norm %>%
    #summarise across subjects for each draw
    group_by(goal_type,.draw) %>%
    summarise(g = mean(g,na.rm=T)) %>%
    #summarise across draws
    group_by(goal_type) %>%
    summarise(g = mean(g))

  surface_data = sim[which(sim$d==sim$d[j] & sim$t==sim$t[j]),]
  surface_data$ap = means$g[1]
  surface_data$av = means$g[2]

  return(surface_data)
}

gradients = surface_data %>%
 gather(key=goal_type,value=g,ap:av) %>%
 mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))

mean_surface = ggplot(data=gradients,aes(x=d,y=t)) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50') +
  scale_fill_distiller(palette="Spectral",breaks=seq(0,0.75,0.25),limits=c(0,0.78)) +
  facet_grid(~goal_type) +
  scale_x_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
  scale_y_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
  labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
  theme.goal +
  theme(legend.position = 'bottom',legend.text = element_text(size=8))

#### Individual Gradients ####

#Spatial gradient
sg1 = posts_norm %>%
  select(subject,.draw,goal_type,w1,delta) %>%
  filter(!is.na(w1))

sg2 = expand.grid(goal_type = unique(sg1$goal_type),
            subject = unique(sg1$subject),
            .draw = unique(sg1$.draw),
            d = seq(0.01,0.99,by=0.01))

#Create plot
sg3 = left_join(sg2,sg1) %>%
  mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta)) %>%
  #summarise across draws
  group_by(goal_type,subject,d) %>%
  summarise(sg = mean(sg)) %>%
  #identify max and min of gradient
  group_by(goal_type,subject) %>%
  mutate(decreasing = head(sg,1) > tail(sg,1),
         direction = factor(as.numeric(decreasing),levels=c(0,1),labels=c('Positive','Negative')))

sg = ggplot(sg3) +
  geom_line(aes(x=d,y=sg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~goal_type) + labs(x='Distance to Goal (D)',y='Spatial Gradient',colour="Direction") +
  scale_color_manual(values=gg_color_hue(3)[c(3,1)]) +
  scale_x_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01)) +
  scale_y_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01),limits=c(0,1)) +
  theme(legend.position = "none") + theme.goal

#count number of positive vs negative gradients
print( sg3 %>%
  summarise(positive = !decreasing[1]) %>%
  count(goal_type,positive) %>%
  mutate(prop = n / sum(n)) )

#Temporal gradient
tg1 = posts_norm %>%
  select(subject,.draw,goal_type,w2,tau) %>%
  filter(!is.na(w2))

tg2 = expand.grid(goal_type = unique(tg1$goal_type),
                  subject = unique(tg1$subject),
                  .draw = unique(tg1$.draw),
                  t = seq(0.01,0.99,by=0.01))

#Create plot
tg3 = left_join(tg2,tg1) %>%
  mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau)) %>%
  #summarise across draws
  group_by(goal_type,subject,t) %>%
  summarise(tg = mean(tg)) %>%
  #identify max and min of gradient
  group_by(goal_type,subject) %>%
  mutate(decreasing = head(tg,1) > tail(tg,1),
         direction = factor(as.numeric(decreasing),levels=c(0,1),labels=c('Positive','Negative')))

tg = ggplot(tg3) +
  geom_line(aes(x=t,y=tg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~goal_type) + labs(x='Time to Deadline (T)',y='Temporal Gradient',colour="Direction") +
  scale_color_manual(values=gg_color_hue(3)[1]) +
  scale_x_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01)) +
  scale_y_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01),limits=c(0,1)) +
  theme(legend.position = "none") + theme.goal

#### Spatiotemporal Gradient ####

#examine position of max of spatiotemporal gradient relative to required rates encountered by participants.

# # IMPORT DATA -------
# #
# # load("data/clean/dp_data_expt12.RData")
# # data_bound_expt12 = data_bound
# #
# # load("data/clean/dp_data_expt3.RData")
# # data_bound_expt3 = data_bound
# #
# # data_bound = rbind(data_bound_expt12,data_bound_expt3) %>%
# #   mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2)
# #
# # # PREPARE DATA FOR VECTOR PLOT -------
# #
# data_bound %>% ungroup() %>%
#   filter(phase==1,left_current_distance > 0 , right_current_distance > 0 )   %>%
#   mutate(
#     #deadline was on a different scale in E3. So for E1 and E2 the normalised deadline variable (T) is calculated
#     #by dividing by 91 (1 + the maximum number of days). For E3 the normalised deadline variable is calculated
#     #by dividing by 9 (1 + the maximum number of months).
#     max_dl = case_when(
#       expt == 1 ~ 91,
#       expt == 2 ~ 91,
#       expt >= 3 ~ 9),
#     a_t = ceiling(right_days_remaining) / max_dl,
#     b_t = ceiling(left_days_remaining) / max_dl,
#     #distance was on the same scale in all three experiments. The max distance observed in any experiment was
#     #187. So we normalise the distance variable by dividing by 188.
#     a_d = ceiling(right_current_distance) /188,
#     b_d = ceiling(left_current_distance) /188) %>%
#   summarise(d_min = min(c(a_d,b_d)),
#             d_max = max(c(a_d,b_d)),
#             t_min = min(c(a_t,b_t)),
#             t_max = max(c(a_t,b_t)),
#             dot_min = min(c(a_d / a_t,b_d/b_t)),
#             dot_05 = quantile(c( a_d/a_t,b_d/b_t), 0.05),
#             dot_10 = quantile(c( a_d/a_t,b_d/b_t), 0.1),
#             dot_90 = quantile(c( a_d/a_t,b_d/b_t), 0.9),
#             dot_95 = quantile(c( a_d/a_t,b_d/b_t), 0.95),
#             dot_max = max(a_d / a_t))

# tibble(alpha = 0.9,
#            x = seq(0.2,5,0.1),
#            y = 1/(alpha/x + (1-alpha)*x)) %>%
# ggplot(aes(x=x,y=y)) + geom_line() +
#   scale_x_continuous(trans="log10")

#approximately 90% of the required rates encountered by participants fall within the interval 0.2 to 5.
#alpha < 0.1 is decreasing in this range, alpha > 0.9 is increasing

stg1=posts_norm %>%
  select(subject,.draw,goal_type,w3,alpha,max)

stg2 = expand.grid(goal_type = unique(stg1$goal_type),
                   subject = unique(stg1$subject),
                   .draw = unique(stg1$.draw),
                   d = seq(0.05,0.95,by=0.05),
                   t = seq(0.05,0.95,by=0.05))

stg3 = left_join(stg2,stg1) %>%
  mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         dot = d/t,
         max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
         stg = w3*max / (alpha/dot + (1-alpha)*dot)) %>%
  #summarise across draws
  group_by(subject,goal_type,d,t) %>%
  summarise(stg = mean(stg),
            dot = mean(dot),
            alpha = mean(alpha)) %>%
  arrange(subject,goal_type,d,t) %>%
  mutate(max_stg = max(stg),
         direction_tmp = case_when(
           alpha < 0.1 ~ 1,
           alpha > 0.9 ~ 2,
           alpha >= 0.1 & alpha <= 0.9 ~ 3
         ),
         direction = factor(direction_tmp ,levels=1:3,labels=c('Decreasing','Increasing','Non-monotonic')))


#stg3$max_stg[stg3$stg!=stg3$max_stg] <- NA

stg = ggplot(stg3) +
  geom_line(aes(x=dot,y=stg,group=subject,colour=direction),alpha=0.1) +
  #geom_point(aes(x=dot,y=max_stg,group=subject),colour="darkblue",alpha=0.2,size=0.5) +
  facet_grid(.~goal_type) + labs(x='Required Rate of Progress (D / T)',y='Spatiotemporal Gradient') +
  scale_x_continuous(trans='log10',limits=c(0.25,4)) +
  scale_color_manual(values=gg_color_hue(3)[c(1,3,2)]) +
  scale_y_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01),limits=c(0,1)) +
  #geom_vline(xintercept = 1,linetype="dotted") +
  theme(legend.position = "none") + theme.goal

# tod: 1 = distance = deadline
# tod: if function peaks at less than 1, mostly decreases with expectancy
# tod: if function peaks at greater than 1, mostly increases with expectancy

#count number of positive vs negative gradients
#stg1 %>% mutate(expectancy_increasing = alpha >= 0.5) %>% count(goal_type,expectancy_increasing )

#number of participants in each category
print( stg3 %>%
  group_by(goal_type,subject) %>%
  summarise(direction = direction[1]) %>%
  group_by(goal_type) %>%
  mutate(n_total = n()) %>%
  group_by(goal_type,direction) %>%
  summarise(n = n(),
            prop = n/n_total[1]) )



#variability in max level
stg3 %>%
  filter(stg==max_stg) %>%
  group_by(goal_type) %>%
  summarise(min_peak = min(max_stg),
            max_peak = max(max_stg),
            mean_peak = mean(max_stg),
            sd_peak = sd(max_stg),
            min_dot = min(dot),
            max_dot = max(dot),
            mean_dot = mean(dot),
            sd_dot = sd(dot))  %>%
  data.frame()



gradient_fig = arrangeGrob(
  mean_surface + theme(axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),legend.title=element_text(size=12)),
  sg + theme(axis.title.x = element_text(size=14),axis.title.y = element_text(size=14)),
  tg + theme(axis.title.x = element_text(size=14),axis.title.y = element_text(size=14)),
  stg + theme(axis.title.x = element_text(size=14),axis.title.y = element_text(size=14)),
  heights=c(1.5,1,1,1)
)

grid.arrange(gradient_fig)

ggsave(file=paste0("figures/overall_motivation",label,".png"),plot=gradient_fig,width=6,height=9)


}




### INDIVIDUAL SURFACES ###



sub_means=posts_norm_tmp %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s) %>%
  #start by calculating mean of posterior for each subject
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value)

# compute position of vectors for each D x T combination based on gradients
gradients = left_join(sim,sub_means,by=c("goal_type","subject")) %>%
  #compute level of gradient for each combination and sum across gradients
  mutate( goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')),
          sg_tmp = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
          tg_tmp = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
          sg = replace_na(sg_tmp,0),
          tg = replace_na(tg_tmp,0),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
          g = sg + tg + stg) %>%


posts_norm_expt = posts_norm %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3),
         goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))


for(expt_to_plot in 1:3){
  for(goal_type_to_plot in c('Approach','Avoiadance')){

    posts_norm_tmp = filter(posts_norm_expt,expt==expt_to_plot,goal_type==goal_type_to_plot)

    sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                      t = seq(0.01,0.99,by=0.01),
                      goal_type=goal_type_to_plot)

    #loop through different points in the surface, because it requires too much memory to calculate mean for all points simultaneously
    sub_surface_data <- foreach(j = 1:nrow(sim),.combine='rbind') %dopar% {

      sg = (posts_norm_tmp$delta>=0)*posts_norm_tmp$w1*sim$d[j]^posts_norm_tmp$delta + (posts_norm_tmp$delta<0)*posts_norm_tmp$w1*(1-sim$d[j]^-posts_norm_tmp$delta)
      tg = (posts_norm_tmp$tau>=0)*posts_norm_tmp$w2*sim$t[j]^posts_norm_tmp$tau + (posts_norm_tmp$tau<0)*posts_norm_tmp$w2*(1-sim$t[j]^-posts_norm_tmp$tau)
      stg = posts_norm_tmp$w3*posts_norm_tmp$max / (posts_norm_tmp$alpha*sim$t[j]/sim$d[j] + (1-posts_norm_tmp$alpha)*sim$d[j]/sim$t[j] )
      posts_norm_tmp$g = replace_na(sg,0) + replace_na(tg,0) + stg

      means = posts_norm_tmp %>%
        #summarise across draws for each subject
        group_by(subject) %>%
        summarise(g = mean(g))

      sub_surface_data = expand.grid(d = sim$d[j],
                                     t = sim$t[j],
                                     subject = unique(posts_norm_tmp$subject))
      sub_surface_data$g = means$g

      return(surface_data)
    }


  sub_surfaces = sub_surface_data
  ggplot(aes(x=d,y=t)) +
  facet_wrap(~subject) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50',binwidth=0.1) +
  scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  #facet_grid(~goal_type) +
  scale_x_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
  theme.goal + theme(legend.position = 'bottom',
                     legend.text = element_text(size=8))

  ggsave(sub_surfaces,file=paste0("figures/individual_surfaces_expt",expt_to_plot,"_",tolower(goal_type_to_plot),".pdf"),height=20,width=20)

  }
}

