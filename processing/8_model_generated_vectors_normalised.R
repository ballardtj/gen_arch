rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)
library(gridExtra)
library(grid)

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

#load posteriors
load("data/derived/unnormalised_posteriors.RData")

#Get values of scaling parameter (theta)
posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
  filter(source=="obs") %>%
  group_by(subject,goal_type) %>%
  summarise(mean_theta = mean(s)) %>%
  group_by(goal_type) %>%
  summarise(lower = quantile(mean_theta,0.025),
            upper = quantile(mean_theta,0.975)) %>%
  data.frame()


#create four different sets of plots
#1) across experiments
#2) experiment 1
#3) experiment 2
#4) experiment 3

for(i in 1:4){

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

  if(i == 1){
    posts_norm = posts_norm_tmp %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = ""
  }
  if(i == 2){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w2)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt1"
  }
  if(i == 3){
    posts_norm = posts_norm_tmp %>%
      filter(is.na(w1)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt2"
  }
  if(i == 4){
    posts_norm = posts_norm_tmp %>%
      filter(!is.na(w1) & !is.na(w2)) %>%
      select(subject,.draw,goal_type,alpha,delta,tau,w1,w2,w3,s) %>%
      gather(key=parameter,value=value,alpha:s)
    label = "_expt3"
  }


  #################################################
  ####### MEAN GRADIENTS ACROSS EXPERIMENTS #######
  #################################################

  # generate data for different combinations of D and T
  sim = expand.grid(a_d = seq(0.125,0.875,by=0.15),
                    b_d = seq(0.125,0.875,by=0.15),
                    a_t = seq(0.1,0.9,by=0.8),
                    b_t = seq(0.1,0.9,by=0.8),
                    goal_type=c('ap','av'))

  # calculate mean parameter value for each frame condition
  means=posts_norm %>%
    #start by calculating mean of posterior for each subject
    group_by(parameter,goal_type,subject) %>%
    summarise(value = mean(value)) %>%
    #then compuate mean parameter across subjects
    group_by(parameter,goal_type) %>%
    summarise(value = mean(value,na.rm=T))  %>%
    spread(key=parameter,value=value)

  # set size of vector in figure
  scale = 0.2

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
      #geom_vline(xintercept=0,size=2.5) +
      #geom_hline(yintercept=0,size=2.5) +
      geom_vline(xintercept=0,size=0.5,linetype='longdash') +
      geom_hline(yintercept=0,size=0.5,linetype='longdash') +
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

  ggsave(file=paste0("figures/mean_gradients",label,".pdf"),plot=figure,height=6,width=12)


  #######################################################################
  ### INDIVIDUAL DIFFERENCES IN GRADIENT COMPONENT ACROSS EXPERIMENTS ###
  #######################################################################


  #simulate different combination of D x T for each subject
  sim = expand.grid(a_d = seq(0.125,0.875,by=0.15),
                    b_d = seq(0.125,0.875,by=0.15),
                    a_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                    b_t = c(0.1,0.9,seq(0.125,0.875,by=0.15)),
                    goal_type=c('ap','av'),
                    subject = unique(posts$subject))

  #compute the mean of each parameter for each subject
  sub_means=posts_norm %>%
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
    #geom_vline(xintercept=0,size=2.5) +
    #geom_hline(yintercept=0,size=2.5) +
    geom_vline(xintercept=0,size=0.5,linetype='longdash') +
    geom_hline(yintercept=0,size=0.5,linetype='longdash') +
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
    #geom_vline(xintercept=0,size=2.5) +
    #geom_hline(yintercept=0,size=2.5) +
    geom_vline(xintercept=0,size=0.5,linetype='longdash') +
    geom_hline(yintercept=0,size=0.5,linetype='longdash') +
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
    #geom_vline(xintercept=0,size=2.5) +
    #geom_hline(yintercept=0,size=2.5) +
    geom_vline(xintercept=0,size=0.5,linetype='longdash') +
    geom_hline(yintercept=0,size=0.5,linetype='longdash') +
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
    #geom_vline(xintercept=0,size=2.5) +
    #geom_hline(yintercept=0,size=2.5) +
    geom_vline(xintercept=0,size=0.5,linetype='longdash') +
    geom_hline(yintercept=0,size=0.5,linetype='longdash') +
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

  ggsave(file=paste0("figures/ids_in_each_gradient",label,".pdf"),plot=figure,height=10,width=12)

}











