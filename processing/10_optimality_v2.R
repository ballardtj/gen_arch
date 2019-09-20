#Made this script on 12 Sept to try and plot the optimality of the gradients by looking at the obs - opt gradients
#over the whole distance or time space. This was because the weight parameters don't necessarily reflect the relative influence,
#Sometimes if the shape parameter is very small, the weight can be very high and not have any effect (e.g., temporal gradient)
#in the avoidance condition.



rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(grid)

#theme for plot
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
                    #legend.title=element_text(size=10,face="bold",family="Times"),
                    legend.title.align = 0.5,
                    #legend.text=element_text(size=6,family="Times"),
                    legend.text.align = 0.5,
                    #legend.key=element_rect(color='white',fill='white'),
                    plot.title = element_text(size=24,face="bold",family="Times",hjust=0.5))#,
# legend.key = element_rect(fill = "transparent", colour = "transparent"))


Nsamp = 100
samples=sample(x=1:8000,size=Nsamp)

get_posts= function(fit,model,dataList,samples){
  posts_tmp1 = tidy_draws(fit)

  #if hierarchical, uncenter delta, tau, and alpha
  if(length(grep('mean',names(posts_tmp1)))>0){
    posts_tmp1 = posts_tmp1 %>%
      mutate_at(vars(contains("w1.")),funs(.*w1_sd + w1_mean)) %>%
      mutate_at(vars(contains("w2.")),funs(.*w2_sd + w2_mean))

    #if
    if(length(grep('w3_mean',names(posts_tmp1)))>0){
      posts_tmp1 = posts_tmp1 %>%
        mutate_at(vars(contains("w3.")),funs(.*w3_sd + w3_mean))
    }
  }

  posts_tmp2 = posts_tmp1 %>%
    #filter out means, sds, and log probability
    select(-contains('mean'),-contains('sd'),-lp__) %>%
    gather_variables() %>%
    #trim to subset
    filter(.draw %in% samples) %>%
    mutate(number = as.numeric(gsub(".*\\.","",as.character(.variable))),
           parameter = gsub("\\..*","",as.character(.variable))) %>%
    ungroup()

  #change parameter number to subject number
  posts_sg = filter(posts_tmp2,parameter %in% c('w1','delta'))  %>%
    mutate(subject = which(dataList$s_sg>0)[number]) %>%
    select(subject,.draw,parameter,.value) %>%
    spread(key=parameter,value=.value) %>%
    mutate(delta = (w1>=0)*delta + (w1<0)*(-delta),
           w1 = abs(w1)) %>%
    gather(key=parameter,value=value,delta:w1)

  posts_tg = filter(posts_tmp2,parameter %in% c('w2','tau'))  %>%
    mutate(subject = which(dataList$s_tg>0)[number]) %>%
    select(subject,.draw,parameter,.value) %>%
    spread(key=parameter,value=.value) %>%
    mutate(tau = (w2>=0)*tau + (w2<0)*(-tau),
           w2 = abs(w2)) %>%
    gather(key=parameter,value=value,tau:w2)

  posts_stg = filter(posts_tmp2,parameter %in% c('w3','alpha')) %>%
    mutate(subject = number,
           value=.value) %>%
    select(subject,.draw,parameter,value)

  posts = bind_rows(posts_sg,posts_tg,posts_stg)
  return(posts)
}

parm_list=list(
  space = c('w1','w2','w3',
            'delta','tau','alpha'),
  nospace = c('w1','w2','delta','tau')
)

frame_labels=c(ap="Approach Condition",av="Avoidance Condition")

post_list=list()
ctr=0
  for (goal_type in c('ap','av')){
    for( source in c('obs','opt')){

        dataList=read_rdump(paste0('data/clean/',source,'_',goal_type,'_rdump_expt123.R'))

        load(paste0("data/derived/expt123_",source,"_",goal_type,"_fit.RData"))

        ctr=ctr+1
        posts = get_posts(fit,model=NA,dataList,samples)
        posts$goal_type = goal_type
        posts$source = source
        post_list[[ctr]] = posts
    }
}


posts = bind_rows(post_list)
rm(post_list)


#calculate normalised parameters
posts_norm_tmp = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s)


#create four different sets of plots
#1) across experiments
#2) experiment 1
#3) experiment 2
#4) experiment 3

for(i in 1:4){


  if(i == 1){
    posts_norm = posts_norm_tmp %>%
      select(subject,.draw,source,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = ""
    print("Combined")
  }
  if(i == 2){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w2)) %>%
      select(subject,.draw,source,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt1"
    print("Experiment 1")
  }
  if(i == 3){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w1)) %>%
      select(subject,.draw,source,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt2"
    print("Experiment 2")
  }
  if(i == 4){
    posts_norm = posts_norm_tmp %>%
      filter(!is.na(w1) & !is.na(w2)) %>%
      select(subject,.draw,source,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt3"
    print("Experiment 3")
  }

  # calculate mean parameter value for each frame condition
  means=posts_norm %>%
    #start by calculating mean of posterior for each subject
    group_by(parameter,goal_type,subject,source) %>%
    summarise(value = mean(value)) %>%
    #then compuate mean parameter across subjects
    group_by(parameter,goal_type,source) %>%
    summarise(value = mean(value,na.rm=T))  %>%
    spread(key=parameter,value=value)

  ### MEAN SURFACES ###

  # generate data for different combinations of D and T
  sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                    t = seq(0.01,0.99,by=0.01),
                    goal_type=c('ap','av'),
                    source = c('obs','opt'))

  #calculate strength of gradint and each combination of D and T
  gradients = left_join(sim,means) %>%
    mutate(w1 = replace_na(w1,0),
           w2 = replace_na(w2,0),
           delta = replace_na(delta,0),
           tau = replace_na(tau,0),
           sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
           tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
           max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
           stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
           g = sg + tg + stg,
           goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance'))) %>%
    select(d,t,goal_type,source,g) %>%
    spread(key=source,value=g) %>%
    mutate(g = obs-opt)


  mean_surface = ggplot(data=gradients,aes(x=d,y=t)) +
    geom_raster(aes(fill=g)) +
    geom_contour(aes(z=g),colour='gray50',binwidth=0.05) +
    scale_fill_distiller(palette="Spectral",breaks=seq(-1,1,0.25),limits=c(-0.6,0.6)) +
    facet_grid(~goal_type) +
    scale_x_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
    scale_y_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
    labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
    theme.goal +
    theme(legend.position = 'bottom',legend.text = element_text(size=8))

  #### Individual Gradients ####

  #Emulate ggplot colour palette
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  #Spatial gradient
  sg1 = posts_norm %>%
    filter(parameter %in% c('w1','delta'),!is.na(parameter)) %>%
    group_by(parameter,goal_type,source,subject) %>%
    summarise(value = mean(value)) %>%
    spread(key=parameter,value=value) %>%
    select(goal_type,source,subject,delta,w1)

  sg2 = expand.grid(goal_type = unique(sg1$goal_type),
                    source = unique(sg1$source),
                    subject = unique(sg1$subject),
                    d = seq(0.01,0.99,by=0.01))

  #Create plot
  sg = left_join(sg2,sg1) %>%
    mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
           sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta)) %>%
    select(goal_type,source,subject,d,sg) %>%
    spread(key=source,value=sg) %>%
    mutate(sg = obs-opt) %>%


        #   direction = factor(as.numeric(delta>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
    ggplot() +
    geom_line(aes(x=d,y=sg,group=subject),alpha=0.2) +
    facet_grid(.~goal_type) + labs(x='Distance to Goal (D)',y='Spatial Gradient',colour="Direction") +
    scale_color_manual(values=gg_color_hue(3)[c(3,1)]) +
    scale_x_continuous(breaks=seq(-0.9,0.9,0.2),expand=c(0.01,0.01)) +
    scale_y_continuous(breaks=seq(-0.9,0.9,0.2),expand=c(0.01,0.01),limits=c(-1,1)) +
    geom_hline(yintercept=0) +
    theme(legend.position = "none") + theme.goal

  #count number of positive vs negative gradients
  print( sg1 %>% mutate(positive = delta >= 0) %>% count(goal_type,positive) %>% mutate(prop = n / sum(n)) )

  #Temporal gradient
  tg1=posts_norm %>%
    filter(parameter %in% c('w2','tau'),!is.na(parameter)) %>%
    group_by(parameter,source,goal_type,subject) %>%
    summarise(value = mean(value)) %>%
    spread(key=parameter,value=value) %>%
    select(goal_type,source,subject,tau,w2)

  tg2 = expand.grid(goal_type = unique(tg1$goal_type),
                    source = unique(tg1$source),
                    subject = unique(tg1$subject),
                    t = seq(0.01,0.99,by=0.01))

  tg = left_join(tg2,tg1) %>%
    mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
           tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau)) %>%
    select(goal_type,source,subject,t,tg) %>%
    spread(key=source,value=tg) %>%
    mutate(tg = obs-opt) %>%
           #direction = factor(as.numeric(tau>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
    ggplot() +
    geom_line(aes(x=t,y=tg,group=subject),alpha=0.2) +
    facet_grid(.~goal_type) + labs(x='Time to Deadline (T)',y='Temporal Gradient',colour="Direction") +
    scale_color_manual(values=gg_color_hue(3)[1]) +
    scale_x_continuous(breaks=seq(-0.9,0.9,0.2),expand=c(0.01,0.01)) +
    scale_y_continuous(breaks=seq(-0.9,0.9,0.2),expand=c(0.01,0.01),limits=c(-1,1)) +
    theme(legend.position = "none") + theme.goal

  #### Spatiotemporal Gradient ####

  #examine position of max of spatiotemporal gradient relative to required rates encountered by participants.

  # IMPORT DATA -------
  #
  # load("data/clean/dp_data_expt12.RData")
  # data_bound_expt12 = data_bound
  #
  # load("data/clean/dp_data_expt3.RData")
  # data_bound_expt3 = data_bound
  #
  # data_bound = rbind(data_bound_expt12,data_bound_expt3) %>%
  #   mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2)
  #
  # # PREPARE DATA FOR VECTOR PLOT -------
  #
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
  #   summarise(dot_min = min(c(a_d / a_t,b_d/b_t)),
  #             dot_05 = quantile(c( a_d/a_t,b_d/b_t), 0.05),
  #             dot_10 = quantile(c( a_d/a_t,b_d/b_t), 0.1),
  #             dot_90 = quantile(c( a_d/a_t,b_d/b_t), 0.9),
  #             dot_95 = quantile(c( a_d/a_t,b_d/b_t), 0.95),
  #             dot_max = max(a_d / a_t))
  #
  # tibble(alpha = 0.9,
  #            x = seq(0.2,5,0.1),
  #            y = 1/(alpha/x + (1-alpha)*x)) %>%
  # ggplot(aes(x=x,y=y)) + geom_line() +
  #   scale_x_continuous(trans="log10")

  #approximately 90% of the required rates encountered by participants fall within the interval 0.2 to 5.
  #alpha < 0.1 is decreasing in this range, alpha > 0.9 is increasing

  stg1=posts_norm %>%
    filter(parameter %in% c('w3','alpha'),!is.na(parameter)) %>%
    group_by(parameter,source,goal_type,subject) %>%
    summarise(value = mean(value)) %>%
    spread(key=parameter,value=value) %>%
    select(goal_type,source,subject,alpha,w3)

  stg2 = expand.grid(goal_type = unique(stg1$goal_type),
                     source = unique(stg1$source),
                     subject = unique(stg1$subject),
                     d = seq(0.05,0.95,by=0.05),
                     t = seq(0.05,0.95,by=0.05))

  stg3 = left_join(stg2,stg1) %>%
    mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
           dot = d/t,
           max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
           stg = w3*max / (alpha/dot + (1-alpha)*dot)) %>%
    group_by(subject,goal_type) %>%
    arrange(subject,goal_type,d,t) %>%
    mutate(max_stg = max(stg),
           # is_max_stg = stg==max_stg,
           # dot_at_max_stg = sum(is_max_stg * dot) / sum(is_max_stg),
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

  ggsave(file=paste0("figures/overall_motivation_relative_to_opt",label,".png"),plot=gradient_fig,width=6,height=9)


}









#transform parameters here
posts_norm = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  select(subject,.draw,source,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

# #correlation between the parameters
# library(GGally)
#
# posts %>%
#   spread(key = parameter,value=value) %>%
#   mutate(expt = case_when(
#     is.na(w2) ~ 1,
#     is.na(w1) ~ 2,
#     !is.na(w1) & !is.na(w2) ~ 3)) %>%
#   gather(key = parameter,value = value,alpha:w3) %>%
#   group_by(subject,source,goal_type,parameter) %>%
#   mutate(mean_value = mean(value),
#          value_mc = value - mean_value) %>%
#   filter(goal_type=="av",source=="obs",expt==3) %>%
#   select(expt,subject,.draw,parameter,value_mc) %>%
#   spread(key = parameter,value=value_mc) %>%
#   ggpairs(.,columns=6:11)
#
# #Weights don't correlate with curvature parameters. No real correlations accept normalised weight (which has to be the case)
#
#
#
#
# #mean across experiment
# across_expt = posts_norm %>%
#   filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
#   spread(key = parameter,value=value) %>%
#   mutate(expt = case_when(
#     is.na(w2) ~ 1,
#     is.na(w1) ~ 2,
#     !is.na(w1) & !is.na(w2) ~ 3)) %>%
#   gather(key = parameter,value = value,w1:w3) %>%
#   spread(key = source, value = value) %>%
#   mutate(bias = obs - opt) %>%
#   group_by(subject,goal_type,parameter) %>%
#   summarise(subject_mean = mean(bias,na.rm=T)) %>%
#   group_by(goal_type,parameter) %>%
#   summarise(upper = quantile(subject_mean,.975,na.rm=T),
#             lower = quantile(subject_mean,.025,na.rm=T),
#             mean = mean(subject_mean,na.rm=T))
#
# print(across_expt)
#
#
# #mean within each experiment
# within_expt = posts_norm %>%
#   filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
#   spread(key = parameter,value=value) %>%
#   mutate(expt = case_when(
#     is.na(w2) ~ 1,
#     is.na(w1) ~ 2,
#     !is.na(w1) & !is.na(w2) ~ 3)) %>%
#   gather(key = parameter,value = value,w1:w3) %>%
#   spread(key = source, value = value) %>%
#   mutate(bias = obs - opt) %>%
#   group_by(subject,goal_type,parameter,expt) %>%
#   summarise(subject_mean = mean(bias,na.rm=T)) %>%
#   group_by(goal_type,parameter,expt) %>%
#   summarise(upper = quantile(subject_mean,.975,na.rm=T),
#             lower = quantile(subject_mean,.025,na.rm=T),
#             mean = mean(subject_mean,na.rm=T))
#
# print(within_expt)

pd = posts_norm %>%
  #filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
  group_by(parameter,goal_type,source) %>%
  summarise(value = mean(value,na.rm=T)) %>% data.frame()

pd = posts_norm %>%
  filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
  spread(key = parameter,value=value) %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3)) %>%
  gather(key = parameter,value = value,w1:w3) %>%
  spread(key = source, value = value) %>%
  mutate(bias = obs - opt) %>%
  group_by(subject,goal_type,parameter,expt) %>%
  summarise(upper = quantile(bias,.975,na.rm=T),
            lower = quantile(bias,.025,na.rm=T),
            mean = mean(bias,na.rm=T))

# fig = pd %>% mutate(jitter = runif(1)) %>%
#   ggplot(aes(x=jitter,y=mean,colour=factor(expt))) +
#   geom_point(alpha=0.5) +
#   geom_errorbar(aes(ymin=lower,ymax=upper),width=0,size=0.2,alpha=0.5) +
#   facet_grid(goal_type~parameter) +
#   labs(x = "",y="Observed Weight minus Optimal Weight",colour="Experiment") +
#   theme(axis.text.x = element_blank())

#number of participants in each category
sub_data = pd %>%
  mutate(category = case_when(
    lower > 0 ~ "overweight",
    upper < 0 ~ "underweight",
    lower < 0 & upper > 0 ~ "neither")) %>%
  mutate(jitter = expt*1.2 + runif(1)) %>%
  filter(!is.na(category))%>%
  ungroup() %>%
  mutate(parameter = factor(parameter,levels=c("w1","w2","w3"),labels=c('Spatial','Temporal','Spatiotemporal')),
         goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))

expt_data = posts_norm %>%
  filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
  spread(key = parameter,value=value) %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3)) %>%
  gather(key = parameter,value = value,w1:w3) %>%
  spread(key = source, value = value) %>%
  mutate(bias = obs - opt) %>%
  group_by(goal_type,parameter,expt,.draw) %>%
   summarise(mean_bias = mean(bias)) %>%
  ungroup() %>%
  mutate(parameter = factor(parameter,levels=c("w1","w2","w3"),labels=c('Spatial','Temporal','Spatiotemporal')),
         goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))

ggplot(sub_data) +
    geom_hline(aes(yintercept = 0),color="gray50") +
  geom_point(aes(x=jitter,y=mean,colour=factor(category)),alpha=0.5,size=0.2) +
  geom_errorbar(aes(x=jitter,ymin=lower,ymax=upper,colour=factor(category)),width=0,size=0.1,alpha=0.5) +
  geom_violin(data=expt_data,aes(x=expt*1.2+0.5,y=mean_bias,group=expt),alpha=0.5,size=1) +
  #geom_errorbar(data=expt_data,aes(x=expt*1.2+0.5,ymin=lower,ymax=upper)) +
  facet_grid(goal_type~parameter) +
  labs(x = "",y="Observed Weight minus Normative Weight",colour="Bias") +
  scale_colour_manual(values=c("black","blue","red"),labels=c('Unbiased','Over-weighted','Under-weighted')) +
  scale_x_continuous(name="Experiment",breaks=c(1.7,2.9,4.1),labels=c("1","2","3")) +
  theme.goal + theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size = 1)))

  ggsave(file="figures/observed_vs_normative.pdf",width=11,height=8)

  #Credible intervals on mean parameters
  group_by(expt_data,expt,goal_type,parameter) %>%
    summarise(lower_ci = quantile(mean_bias,0.025,na.rm=T),
              upper_ci = quantile(mean_bias,0.975,na.rm=T),
              num_na = sum(is.na(mean_bias)))


  #Number of participants in each category

  #Broken down by expt
  group_by(sub_data,expt,goal_type,parameter,category) %>%
    summarise(n = n()) %>%
    group_by(goal_type,parameter) %>%
    mutate(prop = n/sum(n)) %>% data.frame()

  #Across expts
  group_by(sub_data,expt,goal_type,parameter,category) %>%
    summarise(n = n()) %>%
    group_by(goal_type,parameter) %>%
    mutate(prop = n/sum(n)) %>% data.frame()


