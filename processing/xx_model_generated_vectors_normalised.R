rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(grid)

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

        dataList=read_rdump(paste0('data/clean/obs_',goal_type,'_rdump_expt123.R'))

        load(paste0("data/derived/expt123_obs_",goal_type,"_fit.RData"))

        ctr=ctr+1
        posts = get_posts(fit,model=NA,dataList,samples)
        posts$goal_type = goal_type
        post_list[[ctr]] = posts
}


posts = bind_rows(post_list)
rm(post_list)








#transform parameters here
posts_norm = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_exp1 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  filter(is.na(w2)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_exp2 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  filter(is.na(w1)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_exp3 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  filter(!is.na(w1) & !is.na(w2)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_23 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate( s = case_when(
      is.na(w2) ~ w1 + w3,
      is.na(w1) ~ w2 + w3,
      !is.na(w1) & !is.na(w2) ~ w1 + w2 + w3),
          w1 = case_when(
        is.na(w2) ~ w1 / s * 2,
        !is.na(w1) & !is.na(w2) ~ w1 / s *3),
          w2 = case_when(
        is.na(w1) ~ w2 / s * 2,
        !is.na(w1) & !is.na(w2) ~ w2 / s *3),
          w3 = case_when(
        is.na(w2) ~ w3 / s * 2,
        is.na(w1) ~ w3 / s * 2,
        !is.na(w1) & !is.na(w2) ~ w3 / s * 3)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_23_exp1 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate( s = case_when(
    is.na(w2) ~ w1 + w3,
    is.na(w1) ~ w2 + w3,
    !is.na(w1) & !is.na(w2) ~ w1 + w2 + w3),
    w1 = case_when(
      is.na(w2) ~ w1 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w1 / s *3),
    w2 = case_when(
      is.na(w1) ~ w2 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w2 / s *3),
    w3 = case_when(
      is.na(w2) ~ w3 / s * 2,
      is.na(w1) ~ w3 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w3 / s * 3)) %>%
  filter(is.na(w2)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_23_exp2 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate( s = case_when(
    is.na(w2) ~ w1 + w3,
    is.na(w1) ~ w2 + w3,
    !is.na(w1) & !is.na(w2) ~ w1 + w2 + w3),
    w1 = case_when(
      is.na(w2) ~ w1 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w1 / s *3),
    w2 = case_when(
      is.na(w1) ~ w2 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w2 / s *3),
    w3 = case_when(
      is.na(w2) ~ w3 / s * 2,
      is.na(w1) ~ w3 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w3 / s * 3)) %>%
  filter(is.na(w1)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)

posts_norm_23_exp3 = posts %>%
  spread(key=parameter,value=value) %>%
  mutate( s = case_when(
    is.na(w2) ~ w1 + w3,
    is.na(w1) ~ w2 + w3,
    !is.na(w1) & !is.na(w2) ~ w1 + w2 + w3),
    w1 = case_when(
      is.na(w2) ~ w1 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w1 / s *3),
    w2 = case_when(
      is.na(w1) ~ w2 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w2 / s *3),
    w3 = case_when(
      is.na(w2) ~ w3 / s * 2,
      is.na(w1) ~ w3 / s * 2,
      !is.na(w1) & !is.na(w2) ~ w3 / s * 3)) %>%
  filter(!is.na(w1) & !is.na(w2)) %>%
  select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
  gather(key=parameter,value=value,alpha:s)





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
                    #legend.title=element_text(size=10,face="bold",family="Times"),
                    legend.title.align = 0.5,
                    #legend.text=element_text(size=6,family="Times"),
                    legend.text.align = 0.5,
                    #legend.key=element_rect(color='white',fill='white'),
                    plot.title = element_text(size=24,face="bold",family="Times",hjust=0.5))#,
# legend.key = element_rect(fill = "transparent", colour = "transparent"))

### MEAN GRADIENTS ###

# generate data for different combinations of D and T
sim = expand.grid(a_d = seq(0.125,0.875,by=0.15),
                  b_d = seq(0.125,0.875,by=0.15),
                  a_t = seq(0.1,0.9,by=0.8),
                  b_t = seq(0.1,0.9,by=0.8),
                  goal_type=c('ap','av'))

# calculate mean parameter value for each frame condition
means=posts_norm_23_exp2 %>%
  #start by calculating mean of posterior for each subject
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  #then compuate mean parameter across subjects
  group_by(parameter,goal_type) %>%
  summarise(value = mean(value,na.rm=T))  %>%
  spread(key=parameter,value=value)

# set size of vector in figure
scale = 0.15

# compute position of vectors for each D x T combination based on gradients
gradients = left_join(sim,means,by="goal_type") %>%
  #compute level of gradient for each combination and sum across gradients
  mutate( goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          a_sg = (delta>=0)*w1*a_d^delta + (delta<0)*w1*(1-a_d^-delta),
          a_tg = (tau>=0)*w2*a_t^tau + (tau<0)*w2*(1-a_t^-tau),
          b_sg = (delta>=0)*w1*b_d^delta + (delta<0)*w1*(1-b_d^-delta),
          b_tg = (tau>=0)*w2*b_t^tau + (tau<0)*w2*(1-b_t^-tau),
          a_stg = w3*max / (alpha*a_t/a_d + (1-alpha)*a_d/a_t ) ,
          b_stg = w3*max / (alpha*b_t/b_d + (1-alpha)*b_d/b_t ) ,
          a_g = a_sg + a_tg + a_stg,
          b_g = b_sg + b_tg + b_stg) %>%
  gather(key,value,a_sg:b_g) %>%
  separate(key,into=c('goal','gradient')) %>%
  spread(key=goal,value=value) %>%
  #compute position of vector based on gradient level
  mutate(xend = a_d - (goal_type=="Approach")*a*scale + (goal_type=="Avoidance")*a*scale,
         yend = b_d - (goal_type=="Approach")*b*scale + (goal_type=="Avoidance")*b*scale,
         a_tF = factor(a_t,levels=c(0.1,0.9),labels=c("Left: T = 0.1","Left: T = 0.9")),
         b_tF = factor(b_t,levels=c(0.1,0.9),labels=c("Right: T = 0.1","Right: T = 0.9")),
         gradient = factor(gradient,levels=c('sg','tg','stg'),labels=c('Spatial','Temporal','Spatiotemporal')))

plot_mean_gradients = function(data,goal_type_to_plot){

filter(data,goal_type==goal_type_to_plot,gradient!="g") %>%
  ggplot( aes(y = b_d, x = a_d ,group=factor(gradient),colour=as.factor(gradient))) +
  # geom_raster() +
  geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.02, "npc")),alpha=0.8) +
  facet_grid(b_tF~ a_tF ) +
  ylab('Left Distance to Goal (D)') +
  xlab("Right Distance to Goal (D)") +
  #theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
  labs(color = "Gradient") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  theme.goal +
  theme(legend.position = "bottom") +
  ggtitle(goal_type_to_plot)

}

#function to extract legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

ap_plot = plot_mean_gradients(data=gradients,goal_type_to_plot="Approach")
av_plot = plot_mean_gradients(data=gradients,goal_type_to_plot="Avoidance")
legend = g_legend(ap_plot)

figure = grid.arrange(
            arrangeGrob(ap_plot + theme(legend.position="none"),
                        av_plot + theme(legend.position="none"),
                        nrow=1),
            legend,
            nrow=2,
            heights=c(10,1)
)

ggsave(file="figures/mean_gradients_norm_23_exp2.pdf",plot=figure,height=6,width=12)

### Individual differences in gradient components ###

#simulate different combination of D x T for each subject
sim = expand.grid(a_d = seq(0.125,0.875,by=0.15),
                  b_d = seq(0.125,0.875,by=0.15),
                  a_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                  b_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                  goal_type=c('ap','av'),
                  subject = unique(posts$subject))

#compute the mean of each parameter for each subject
sub_means=posts_norm_23_exp2 %>%
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value)

#set scale of vector size
scale = 0.1

#function to emulate the ggplot colour pallette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# compute position of vectors for each D x T combination based on gradients
gradients = left_join(sim,sub_means,by=c("goal_type","subject")) %>%
  #compute level of gradient for each combination and sum across gradients
  mutate( goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          a_sg = (delta>=0)*w1*a_d^delta + (delta<0)*w1*(1-a_d^-delta),
          a_tg = (tau>=0)*w2*a_t^tau + (tau<0)*w2*(1-a_t^-tau),
          b_sg = (delta>=0)*w1*b_d^delta + (delta<0)*w1*(1-b_d^-delta),
          b_tg = (tau>=0)*w2*b_t^tau + (tau<0)*w2*(1-b_t^-tau),
          a_stg = w3*max / (alpha*a_t/a_d + (1-alpha)*a_d/a_t ) ,
          b_stg = w3*max / (alpha*b_t/b_d + (1-alpha)*b_d/b_t ) ,
          a_g = a_sg + a_tg + a_stg,
          b_g = b_sg + b_tg + b_stg) %>%
  gather(key,value,a_sg:b_g) %>%
  separate(key,into=c('goal','gradient')) %>%
  spread(key=goal,value=value) %>%
  #create variable indicating the experiment
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3
  )) %>%
  #compute position of vector based on gradient level
  mutate(xend = a_d - (goal_type=="Approach")*a*scale + (goal_type=="Avoidance")*a*scale,
         yend = b_d - (goal_type=="Approach")*b*scale + (goal_type=="Avoidance")*b*scale,
         xend_t = a_t - (goal_type=="Approach")*a*scale + (goal_type=="Avoidance")*a*scale,
         yend_t = b_t - (goal_type=="Approach")*b*scale + (goal_type=="Avoidance")*b*scale,
         a_tF = factor(a_t,levels=c(0.1,0.9),labels=c("Left: T = 0.1","Left: T = 0.9")),
         b_tF = factor(b_t,levels=c(0.1,0.9),labels=c("Right: T = 0.1","Right: T = 0.9")),
         gradient = factor(gradient,levels=c('sg','tg','stg'),labels=c('Spatial','Temporal','Spatiotemporal')))

#Spatial
spatial_id_plot = filter(gradients,a_t==0.1,b_t==0.1,gradient=="Spatial") %>%
  ggplot( aes(y = b_d, x = a_d)) +# ,group=factor(gradient),colour=as.factor(gradient))) +
  # geom_raster() +
  #  geom_point() +
  geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5,color=gg_color_hue(3)[1]) +
  facet_grid(.~ goal_type ) +
  ylab('Left Distance to Goal (D)') +
  xlab("Right Distance to Goal (D)") +
  #theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
  labs(color = "Gradient") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  theme.goal +
  theme(legend.position = "none")  +
  coord_cartesian(xlim=c(-0.1,1),ylim=c(-0.1,1)) +
  ggtitle("Spatial Gradient")


#Temporal
temporal_id_plot = filter(gradients,a_d==0.125,b_d==0.125,a_t!=0.1,a_t!=0.9,b_t!=0.1,b_t!=0.9,gradient=="Temporal") %>%
  ggplot( aes(y = b_t, x = a_t ))  +# ,group=factor(gradient),colour=as.factor(gradient))) +
  # geom_raster() +
  #  geom_point() +
  geom_segment(aes(xend=xend_t,yend=yend_t),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5,color=gg_color_hue(3)[2]) +
  facet_grid(.~ goal_type ) +
  ylab('Left Time to Deadline (T)') +
  xlab("Right Time to Deadline (T)") +
  #theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
  labs(color = "Gradient") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  theme.goal +
  theme(legend.position = "none") +
  coord_cartesian(xlim=c(-0.1,1),ylim=c(-0.1,1)) +
  ggtitle("Temporal Gradient")


#Spatiotemporal
spatiotemporal_ap_id_plot = filter(gradients,!is.na(a_tF),!is.na(b_tF),gradient=="Spatiotemporal",goal_type=="Approach") %>%
  ggplot( aes(y = b_d, x = a_d )) +
  # geom_raster() +
  #  geom_point() +
  geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5,color=gg_color_hue(3)[3]) +
  facet_grid(b_tF~ a_tF ) +
  ylab('Left Distance to Goal (D)') +
  xlab("Right Distance to Goal (D)") +
  #theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
  labs(color = "Gradient") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  theme.goal +
  theme(legend.position = "none",plot.title = element_text(size=18)) +
  ggtitle("Approach")+
  coord_cartesian(xlim=c(-0.1,1),ylim=c(-0.1,1))

spatiotemporal_av_id_plot = filter(gradients,!is.na(a_tF),!is.na(b_tF),gradient=="Spatiotemporal",goal_type=="Avoidance") %>%
  ggplot( aes(y = b_d, x = a_d )) +
  # geom_raster() +
  #  geom_point() +
  geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5,color=gg_color_hue(3)[3]) +
  facet_grid(b_tF~ a_tF ) +
  ylab('Left Distance to Goal (D)') +
  xlab("Right Distance to Goal (D)") +
  #theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
  labs(color = "Gradient") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  theme.goal +
  theme(legend.position = "none",plot.title = element_text(size=18)) +
  ggtitle("Avoidance")+
  coord_cartesian(xlim=c(-0.1,1),ylim=c(-0.1,1))

figure = grid.arrange(
  arrangeGrob(spatial_id_plot,temporal_id_plot,nrow=1),
  arrangeGrob(spatiotemporal_ap_id_plot ,spatiotemporal_av_id_plot,nrow=1,
              top=textGrob("Spatiotemporal Gradient", gp=gpar(fontsize=24,fontface="bold",fontfamily = "Times"))),
  nrow=2,
  heights=c(1,2)
)

ggsave(file="figures/ids_in_each_gradient_norm_23_exp2.pdf",plot=figure,height=10,width=12)


#Overall

# filter(gradients,!is.na(a_tF),!is.na(b_tF),is.na(gradient),goal_type=="Avoidance") %>%
#   ggplot( aes(y = b_d, x = a_d )) +
#   # geom_raster() +
#   #  geom_point() +
#   geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5) +
#   facet_grid(b_tF~ a_tF ) +
#   ylab('Left Distance to Goal (D)') +
#   xlab("Right Distance to Goal (D)") +
#   #theme.goal +
#   #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
#   #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
#   labs(color = "Gradient") +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   geom_vline(xintercept=0,size=2.5) +
#   geom_hline(yintercept=0,size=2.5) +
#   geom_vline(xintercept=0,size=0.5,color='yellow') +
#   geom_hline(yintercept=0,size=0.5,color='yellow') +
#   theme.goal +
#   theme(legend.position = "bottom")


### MEAN SURFACES ###

# generate data for different combinations of D and T
sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  goal_type=c('ap','av'))

#calculate strength of gradint and each combination of D and T
gradients = left_join(sim,means) %>%
  mutate( sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
          tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
          g = sg + tg + stg,
          goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))

mean_surface = ggplot(data=gradients,aes(x=d,y=t)) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
  scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  facet_grid(~goal_type) +
  scale_x_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
  scale_y_continuous(breaks=seq(0.1,0.9,by=0.2),expand=c(0.01,0.01)) +
  labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
  theme.goal +
  theme(legend.position = 'bottom')

#### Individual Gradients ####

#Emulate ggplot colour palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#Spatial gradient
sg1 = posts %>%
  filter(parameter %in% c('w1','delta'),!is.na(parameter)) %>%
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(goal_type,subject,delta,w1)

sg2 = expand.grid(goal_type = unique(sg1$goal_type),
            subject = unique(sg1$subject),
            d = seq(0.01,0.99,by=0.01))

#Create plot
sg = left_join(sg2,sg1) %>%
  mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
         direction = factor(as.numeric(delta>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
  ggplot() +
  geom_line(aes(x=d,y=sg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~goal_type) + labs(x='Distance to Goal (D)',y='Spatial Gradient',colour="Direction") +
  scale_color_manual(values=gg_color_hue(3)[c(3,1)]) +
  scale_x_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01)) +
  theme(legend.position = "none") + theme.goal

#count number of positive vs negative gradients
sg1 %>% mutate(positive = delta >= 0) %>% count(goal_type,positive) %>% mutate(prop = n / sum(n))

#Temporal gradient
tg1=posts %>%
  filter(parameter %in% c('w2','tau'),!is.na(parameter)) %>%
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(goal_type,subject,tau,w2)

tg2 = expand.grid(goal_type = unique(tg1$goal_type),
                  subject = unique(tg1$subject),
                  t = seq(0.01,0.99,by=0.01))

tg = left_join(tg2,tg1) %>%
  mutate(goal_type = factor(goal_type,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
         direction = factor(as.numeric(tau>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
  ggplot() +
  geom_line(aes(x=t,y=tg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~goal_type) + labs(x='Time to Deadline (T)',y='Temporal Gradient',colour="Direction") +
  scale_color_manual(values=gg_color_hue(3)[1]) +
  scale_x_continuous(breaks=seq(0.1,0.9,0.2),expand=c(0.01,0.01)) +
  theme(legend.position = "none") + theme.goal

#### Spatiotemporal Gradient ####

#examine position of max of spatiotemporal gradient relative to required rates encountered by participants.

# IMPORT DATA -------

load("data/clean/dp_data_expt12.RData")
data_bound_expt12 = data_bound

load("data/clean/dp_data_expt3.RData")
data_bound_expt3 = data_bound

data_bound = rbind(data_bound_expt12,data_bound_expt3) %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2)

# PREPARE DATA FOR VECTOR PLOT -------

data_bound %>% ungroup() %>%
  filter(phase==1,left_current_distance > 0 , right_current_distance > 0 )   %>%
  mutate(
    #deadline was on a different scale in E3. So for E1 and E2 the normalised deadline variable (T) is calculated
    #by dividing by 91 (1 + the maximum number of days). For E3 the normalised deadline variable is calculated
    #by dividing by 9 (1 + the maximum number of months).
    max_dl = case_when(
      expt == 1 ~ 91,
      expt == 2 ~ 91,
      expt >= 3 ~ 9),
    a_t = ceiling(right_days_remaining) / max_dl,
    b_t = ceiling(left_days_remaining) / max_dl,
    #distance was on the same scale in all three experiments. The max distance observed in any experiment was
    #187. So we normalise the distance variable by dividing by 188.
    a_d = ceiling(right_current_distance) /188,
    b_d = ceiling(left_current_distance) /188) %>%
  summarise(dot_min = min(c(a_d / a_t,b_d/b_t)),
            dot_05 = quantile(c( a_d/a_t,b_d/b_t), 0.05),
            dot_10 = quantile(c( a_d/a_t,b_d/b_t), 0.1),
            dot_90 = quantile(c( a_d/a_t,b_d/b_t), 0.9),
            dot_95 = quantile(c( a_d/a_t,b_d/b_t), 0.95),
            dot_max = max(a_d / a_t))

#approximately 90% of the required rates encountered by participants fall within the interval 0.2 to 5.

stg1=posts %>%
  filter(parameter %in% c('w3','alpha'),!is.na(parameter)) %>%
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(goal_type,subject,alpha,w3)

stg2 = expand.grid(goal_type = unique(stg1$goal_type),
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
         is_max_stg = stg==max_stg,
         dot_at_max_stg = sum(is_max_stg * dot) / sum(is_max_stg),
         direction_tmp = case_when(
           dot_at_max_stg < 0.35 ~ 1,
           dot_at_max_stg > 3.5 ~ 2,
           dot_at_max_stg >= 0.35 & dot_at_max_stg <= 3.5 ~ 3
         ),
         direction = factor(direction_tmp ,levels=1:3,labels=c('Decreasing','Increasing','Non-monotonic')))


#stg3$max_stg[stg3$stg!=stg3$max_stg] <- NA

stg = ggplot(stg3) +
  geom_line(aes(x=dot,y=stg,group=subject,colour=direction),alpha=0.1) +
  #geom_point(aes(x=dot,y=max_stg,group=subject),colour="darkblue",alpha=0.2,size=0.5) +
  facet_grid(.~goal_type) + labs(x='Required Rate of Progress (D / T)',y='Spatiotemporal Gradient') +
  scale_x_continuous(trans='log10',limits=c(0.25,4)) +
  scale_color_manual(values=gg_color_hue(3)[c(1,3,2)]) +
  #geom_vline(xintercept = 1,linetype="dotted") +
  theme(legend.position = "none") + theme.goal

# tod: 1 = distance = deadline
# tod: if function peaks at less than 1, mostly decreases with expectancy
# tod: if function peaks at greater than 1, mostly increases with expectancy

#count number of positive vs negative gradients
#stg1 %>% mutate(expectancy_increasing = alpha >= 0.5) %>% count(goal_type,expectancy_increasing )

#number of participants in each category
stg3 %>%
  group_by(goal_type,subject) %>%
  summarise(direction = direction[1]) %>%
  group_by(goal_type) %>%
  mutate(n_total = n()) %>%
  group_by(goal_type,direction) %>%
  summarise(n = n(),
            prop = n/n_total[1])



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

ggsave(file=paste0("figures/overall_motivation.png"),plot=gradient_fig,width=6,height=9)







### INDIVIDUAL SURFACES ###

sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  goal_type=c('ap','av'),
                  subject = unique(posts$subject))

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
  #create variable indicating the experiment
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3
  ))

for(expt_to_plot in 1:3){
  for(goal_type_to_plot in c('Approach','Avoidance')){

sub_surfaces = gradients %>%
  filter(expt == expt_to_plot,goal_type==goal_type_to_plot) %>%
  ggplot(aes(x=d,y=t)) +
  facet_wrap(~subject) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
  scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  #facet_grid(~goal_type) +
  scale_x_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
  theme(legend.position = 'bottom') + theme.goal

  ggsave(sub_surfaces,file=paste0("figures/individual_surfaces_expt_",expt_to_plot,"_",tolower(goal_type_to_plot),".pdf"),height=20,width=20)

  }
}

