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

        load(paste0("data/derived/expt123_",goal_type,"_fit.RData"))

        ctr=ctr+1
        posts = get_posts(fit,model=NA,dataList,samples)
        posts$goal_type = goal_type
        post_list[[ctr]] = posts
}


posts = bind_rows(post_list)
rm(post_list)

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
means=posts %>%
  #start by calculating mean of posterior for each subject
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  #then compuate mean parameter across subjects
  group_by(parameter,goal_type) %>%
  summarise(value = mean(value))  %>%
  spread(key=parameter,value=value)

# set size of vector in figure
scale = 0.02

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

ggsave(file="figures/mean_gradients.pdf",plot=figure,height=6,width=12)

### Individual differences in gradient components ###

#simulate different combination of D x T for each subject
sim = expand.grid(a_d = seq(0.125,0.875,by=0.15),
                  b_d = seq(0.125,0.875,by=0.15),
                  a_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                  b_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                  goal_type=c('ap','av'),
                  subject = unique(posts$subject))

#compute the mean of each parameter for each subject
sub_means=posts %>%
  group_by(parameter,goal_type,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value)

#set scale of vector size
scale = 0.02

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

ggsave(file="figures/ids_in_each_gradient.pdf",plot=figure,height=10,width=12)


#Overall

filter(gradients,!is.na(a_tF),!is.na(b_tF),is.na(gradient),goal_type=="Avoidance") %>%
  ggplot( aes(y = b_d, x = a_d )) +
  # geom_raster() +
  #  geom_point() +
  geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.01, "npc")),alpha=0.075,size=0.5) +
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
  theme(legend.position = "bottom")










# Mean across participants of parameters from model of observed decisions
pd1 = posts %>%
  #filter(source=="obs") %>%
  group_by(.draw,parameter,frame) %>%
  summarise(estimate = mean(value))

#Mean parameters across conditions
ggplot(data=pd1) +
  geom_density(aes(x=estimate,fill=frame),alpha=0.5) +
  facet_grid(.~parameter,scale="free")

# Mean across participants of parameters from models of observed and optimal decisions
pd2 = posts %>%
  group_by(.draw,parameter,frame,source) %>%
  summarise(estimate = mean(value))

#Mean parameters across conditions
ggplot(data=pd2) +
  geom_density(aes(x=estimate,fill=source),alpha=0.5) +
  facet_grid(frame~parameter,scale="free")

# 95% CI for each participant of parameters of model of observed decisions

pd3=posts %>%
  filter(source=="obs") %>%
  group_by(parameter,subject,frame,structure,model) %>%
  point_interval(value) %>%
  group_by(parameter,frame,structure,model) %>%
  arrange(value) %>%
  mutate(order = 1:n())

ggplot(data=pd3) +
  geom_pointintervalh(aes(y=order,x=value,colour=interaction(structure,model)),show.legend=T,size=0.1,alpha=0.2) +
  facet_grid(frame~parameter,scale="free") + theme(legend.position="bottom")

#### Individual Gradients ####

#Emulate ggplot colour palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#Spatial gradient
sg1 = posts %>%
  filter(source=="obs",parameter %in% c('w1','delta'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(frame,subject,delta,w1)

sg2 = expand.grid(frame = unique(sg1$frame),
            subject = unique(sg1$subject),
            d = seq(0.01,0.99,by=0.01))

#Create plot
sg = left_join(sg2,sg1) %>%
  mutate(frame = factor(frame,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
         direction = factor(as.numeric(delta>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
  ggplot() +
  geom_line(aes(x=d,y=sg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~frame) + labs(x='Distance to Goal (D)',y='Spatial Gradient',colour="Direction") +
  scale_color_manual(values=rev(gg_color_hue(2))) +
  theme(legend.position = "none")

#count number of positive vs negative gradients
sg1 %>% mutate(positive = delta >= 0) %>% count(frame,positive) %>% mutate(prop = n / sum(n))

#Temporal gradient
tg1=posts %>%
  filter(source=="obs",parameter %in% c('w2','tau'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(frame,subject,tau,w2)

tg2 = expand.grid(frame = unique(tg1$frame),
                  subject = unique(tg1$subject),
                  t = seq(0.01,0.99,by=0.01))

tg = left_join(tg2,tg1) %>%
  mutate(frame = factor(frame,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
         direction = factor(as.numeric(tau>=0),levels=c(1,0),labels=c('Positive','Negative'))) %>%
  ggplot() +
  geom_line(aes(x=t,y=tg,group=subject,colour=direction),alpha=0.2) +
  facet_grid(.~frame) + labs(x='Time to Deadline (T)',y='Temporal Gradient',colour="Direction") +
  #scale_color_manual(values=rev(gg_color_hue(2))) +
  theme(legend.position = "none")

#Spatiotemporal Gradient
stg1=posts %>%
  filter(source=="obs",parameter %in% c('w3','alpha'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  select(frame,subject,alpha,w3)

stg2 = expand.grid(frame = unique(stg1$frame),
                   subject = unique(stg1$subject),
                   d = seq(0.05,0.95,by=0.05),
                   t = seq(0.05,0.95,by=0.05))

stg3 = left_join(stg2,stg1) %>%
  mutate(frame = factor(frame,levels=c('ap','av'),labels=c("Approach","Avoidance")),
         dot = d/t,
         max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
         stg = w3*max / (alpha/dot + (1-alpha)*dot),
         direction = factor(as.numeric(alpha>=0.5),levels=c(1,0),labels=c('Positive','Negative'))) %>%
  group_by(subject,frame) %>%
  arrange(subject,frame,d,t) %>%
  mutate(max_stg = max(stg))

stg3$max_stg[stg3$stg!=stg3$max_stg] <- NA

stg = ggplot(stg3) +
  geom_line(aes(x=dot,y=stg,group=subject),alpha=0.1,colour="black") +
  geom_point(aes(x=dot,y=max_stg,group=subject),colour="darkblue",alpha=0.2,size=0.5) +
  facet_grid(.~frame) + labs(x='Required Rate of Progress (D / T)',y='Spatiotemporal Gradient') +
  scale_x_continuous(trans='log10') +
  #geom_vline(xintercept = 1,linetype="dotted") +
  theme(legend.position = "none")

# tod: 1 = distance = deadline
# tod: if function peaks at less than 1, mostly decreases with expectancy
# tod: if function peaks at greater than 1, mostly increases with expectancy

#count number of positive vs negative gradients
stg1 %>% mutate(expectancy_increasing = alpha >= 0.5) %>% count(frame,expectancy_increasing )

#variability in max level
stg3 %>%
  filter(stg==max_stg) %>%
  group_by(frame) %>%
  summarise(min_peak = min(max_stg),
            max_peak = max(max_stg),
            mean_peak = mean(max_stg),
            sd_peak = sd(max_stg),
            min_dot = min(dot),
            max_dot = max(dot),
            mean_dot = mean(dot),
            sd_dot = sd(dot))  %>%
  data.frame()


#Create surface plots





#1. Showing surface generated using mean parameters.

#2. Showing the max of each individual surface (as a function of approach/avoidance/distance/deadline)


# -w1*(d1^delta - d2^delta)
# -w1*d1^delta - -w1*d2^delta
# w1*d2^delta - w2*d1^delta

# w1*(1-d1^delta) - w1*(1-d2^delta)
# w1 - w1*d1^delta - w1 - w1

# d = seq(0.01,0.99,0.01)
# w1=1
# delta = -0.5
# sg = w1*(1-d)^-delta
# plot(d,sg)


# Surface Plot of Mean Gradients

sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  frame=c('ap','av'))

means=posts %>%
  filter(source=="obs",model=="space",structure=="hier",!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  group_by(parameter,frame) %>%
  summarise(value = mean(value))  %>%
  spread(key=parameter,value=value)

gradients = left_join(sim,means) %>%
  mutate( sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
          tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
          g = sg + tg + stg,
          frame = factor(frame,levels=c('ap','av'),labels=c('Approach','Avoidance')))

surface = ggplot(data=gradients,aes(x=d,y=t)) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
  scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  facet_grid(~frame) +
  scale_x_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  scale_y_continuous(breaks=seq(0,1,by=0.25),expand=c(0.02,0.02)) +
  labs(x='Distance to Goal (D)',y='Time to Deadline (T)',fill='Motivational Value') +
  theme(legend.position = 'bottom')

#Combine plots into figure


gradient_fig = arrangeGrob(
  arrangeGrob(surface), #av_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
  #top=textGrob(expression(italic("Experiment 2, Avoidance Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(sg),
              #ap_pp_plot[[1]] + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              #top=textGrob(expression(italic("Experiment 1, Approach Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(tg), #av_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              #top=textGrob(expression(italic("Experiment 1, Avoidance Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(stg),#ap_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
              #top=textGrob(expression(italic("Experiment 2, Approach Condition")),gp=gpar(fontsize=12))),
  nrow=4,
  heights=c(1.5,1,1,1)
)

grid.arrange(gradient_fig)

ggsave(file=paste0("figures/gradients_expt123.png"),plot=gradient_fig,width=6,height=9)

# Vector plot of gradient components






#Scatterplots of participant parms
library(GGally)

pd1 = posts %>%
  filter(frame=="av") %>%
  group_by(subject,parameter) %>%
  summarise(estimate = mean(value)) %>%
  ungroup() %>%
  spread(key=parameter,value=estimate) %>%
  select(-subject) %>%
  ggpairs(upper=list(continuous="density"))

#alpha by experiment

posts %>%
  filter(frame=="ap") %>%
  group_by(subject,parameter) %>%
  summarise(estimate = mean(value)) %>%
  ungroup() %>%
  spread(key=parameter,value=estimate) %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3
  )) %>%
  gather(key=parameter,value=estimate,alpha:w3) %>%
  ggplot(aes(group=factor(expt),colour=factor(expt))) +
  geom_density(aes(x=estimate),alpha=0.3) +
  facet_wrap(~parameter,scale="free") +
  coord_cartesian(xlim=)

  select(-subject)

pd1


#Mixture modelling of parameters

install.packages('mixAK')
library('mixAK')

data("PBC910", package = "mixAK")

plotProfiles(PBC910)

mod <- GLMM_MCMC(y = PBC910[, c("lbili", "platelet", "spiders")], #data frame with observed values
                 dist = c("gaussian", "poisson(log)", "binomial(logit)"), #assumed distributions and link functions
                 id = PBC910[, "id"], #id variable

                 x = list(lbili = "empty",     # mean structure of each variable, which is a list of fixed effects that predict each variable ('empty' if none)
                          platelet = "empty",
                          spiders = PBC910[, "month"]),

                 z = list(lbili = PBC910[, "month"], #random effects (except intercept term)
                          platelet = PBC910[, "month"],
                          spiders = "empty"),

                 random.intercept = rep(TRUE, 3), #logical argument for random intercept
                 prior.b = list(Kmax = 2),        #maximum number of clusters
                 nMCMC = c(burn = 100, keep = 1000, thin = 10, info = 100),
                 parallel = FALSE)

subject_means = posts %>%
  filter(source=="obs",model=="space",structure=="hier",!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  spread(key=parameter,value=value) %>%
  filter(frame=="ap") %>%
  ungroup() %>%
  select(subject,w3,alpha)

#%>%
  #filter(!is.na(w1),!is.na(w2))


mod <- GLMM_MCMC(y = as.data.frame(subject_means[,2:3]), #data frame with observed values
                 dist = c("gaussian", "gaussian"), #assumed distributions and link functions
                 id = subject_means$subject, #id variable

                 x = list(w3 = "empty",     # mean structure of each variable, which is a list of fixed effects that predict each variable ('empty' if none)
                          alpha = "empty"),

                 z = list(w3 = "empty",     #random effects (except intercept term)
                          alpha = "empty"),

                 random.intercept = rep(FALSE, 2), #logical argument for random intercept
                 prior.b = list(Kmax = 3),        #maximum number of clusters
                 nMCMC = c(burn = 100, keep = 1000, thin = 10, info = 100),
                 parallel = FALSE,
                 PED = FALSE)

#Model is estimating but fails when computing the deviance!

GLMM_MCMC(y=as.matrix(subject_means[,2:7]))




# Scatter Plot of Subject Maximum Gradients

sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  frame=c('ap','av'),
                  subject = 1:max(unique(posts$subject)))

means=posts %>%
  filter(source=="obs",model=="space",structure=="hier",!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value)) %>%
  #group_by(parameter,frame) %>%
  #summarise(value = mean(value))  %>%
  spread(key=parameter,value=value)

gradients = left_join(sim,means,by=c('frame','subject')) %>%
  mutate( sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
          tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
          g = case_when(
            is.na(w2) ~ sg+stg,
            is.na(w1) ~ tg+stg),
          frame = factor(frame,levels=c('ap','av'),labels=c('Approach','Avoidance')))

max_gradients = gradients %>%
  group_by(subject,frame) %>%
  summarise(d = d[g==max(g)],
            t = t[g==max(g)])

ggplot(data=max_gradients,aes(x=d,y=t)) +
  geom_point() +
  #geom_raster(aes(fill=g)) +
  #geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
  #scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  facet_grid(~frame)

# Surface Plots of Individual Gradients

sim = expand.grid(d = seq(0.01,0.99,by=0.01),
                  t = seq(0.01,0.99,by=0.01),
                  frame=c('ap','av'),
                  subject = 1:244)

means=posts %>%
  filter(source=="obs",model=="space",structure=="hier",!is.na(parameter)) %>%
  group_by(parameter,frame,subject) %>%
  summarise(value = mean(value))  %>%
  spread(key=parameter,value=value)

means$w1[is.na(means$w1)] = 0
means$w2[is.na(means$w2)] = 0
means$delta[is.na(means$delta)] = 0
means$tau[is.na(means$tau)] = 0

gradients = left_join(sim,means) %>%
  mutate( sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta),
          tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau),
          max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*t/d + (1-alpha)*d/t ) ,
          g = sg + tg + stg,
          frame = factor(frame,levels=c('ap','av'),labels=c('Approach','Avoidance')))

for(i in 1:244){
  pl=ggplot(data=subset(gradients,subject==i),aes(x=d,y=t)) +
    geom_raster(aes(fill=g)) +
    geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
    scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
    facet_grid(~frame) +
    labs(x = "Distance to Goal",y = "Time to Deadline",fill="Motivation")
  ggsave(file=paste0("analysis/gradients/subject",i,".pdf"),plot=pl,height=6,width=8)
}

# Scatterplot of Spatial Gradient Parameters

means=posts %>%
  filter(source=="obs",model=="space",parameter=="w1"|parameter=="delta") %>%
  select(subject,.draw,frame,parameter,structure,value) %>%
  spread(key=structure,value=value) %>%
  group_by(subject,frame,parameter) %>%
  summarise(hier = mean(hier),
            #delta_l = quantile(delta,0.025),
            #delta_u = quantile(delta,0.975),
            fixed = mean(fixed))
#w1_l = quantile(w1,0.025),
#w1_u = quantile(w1,0.975))

ggplot(data=means) +
  geom_point(aes(x=hier,y=fixed)) +
  facet_grid(parameter~frame,scale="free") +
  labs(x="hierarchical estimate",y="non-hierarchical estimate")


