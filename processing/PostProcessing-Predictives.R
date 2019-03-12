rm(list=ls())
library(tidyverse)

# setwd("~/Dropbox/Research/Projects/MGP-1517-GOAL/Modelling-7-NormalisedModel")

# SET THEME (REQUIRED) ----------------

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
       plot.title = element_text(size=18,face="bold",family="Times",vjust=1))#,
      # legend.key = element_rect(fill = "transparent", colour = "transparent"))



# IMPORT DATA (REQUIRED) -------

load("data/clean/dp_data.RData")

# TRIAL LEVEL PREDICTIVES -----

load("Models.Rda")

models.condition = list(Distance = c(1:20,61:63,67), #distance models
                        Deadline = c(1:60,64:66,68))

for(d in c("Distance",'Deadline')){

  data_obs = data %>% filter(right_current_distance>0.5,left_current_distance>0.5,phase==1,diffCon==d) %>%
    mutate(source = "Observed",
           prioritise_right_upper = NaN,
           prioritise_right_lower = NaN)

  data_pred = data_obs %>%
    mutate(source = "Predicted")

for(m in models.condition[[d]]){
  print(m)
  for(f in c('Approach','Avoidance')){

    file=paste0(Sys.getenv('HOME'),'/Dropbox/Research/Projects/MGP-1517-GOAL/Modelling-5-RealDataHierarchical/Output/',d,f,'_',m,'_',models[m],'/Summary.Rda')
    if( file.exists(file) ){
      load(file)
      data_pred$prioritise_right[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"50%"]))
      data_pred$prioritise_right_upper[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"97.5%"]))
      data_pred$prioritise_right_lower[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"2.5%"]))
    } else {
      data_pred$prioritise_right[data_pred$goal_type==f] = NA
      data_pred$prioritise_right_upper[data_pred$goal_type==f] = NA
      data_pred$prioritise_right_lower[data_pred$goal_type==f] = NA
    }
  }

  data_comb = bind_rows(data_obs,data_pred)

  if(d == "Distance"){
    data_comb$panel_factor = factor(data_comb$left_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Left Start: 30','Left Start: 45','Left Start: 67.5','Left Start: 90','Left Start: 112.5','Left Start: 135','Left Start: 150'))
    data_comb$x_factor = factor(data_comb$right_start_distance,levels=c(30,45,67.5,90,112.5,135,150))
  }
  if(d == "Deadline"){
    data_comb$panel_factor = factor(data_comb$left_start_deadline,levels=c(18,20,24,30,40,60,90),labels=c('Left Deadline: 18','Left Deadline: 20','Left Deadline: 24','Left Deadline: 30','Left Deadline: 40','Left Deadline: 60','Left Deadline: 90'))
    data_comb$x_factor = factor(data_comb$right_start_deadline,levels=c(18,20,24,30,40,60,90))
  }

  # pd <- data_comb %>%
  #   group_by(x_factor,panel_factor,source,goal_type) %>%
  #   summarise(choice_right = mean(prioritise_right),
  #             choice_right_upper = mean(prioritise_right_upper),
  #             choice_right_lower = mean(prioritise_right_lower),
  #             choice_se = sqrt(choice_right*(1-choice_right)/length(stage)))
  # pd$choice_se[pd$source=="Predicted"]<-NA
  #
  # ggplot(data=pd,aes(x=x_factor)) +
  #   geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  #   geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
  #   geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
  #   facet_grid(goal_type~panel_factor) +
  #   coord_cartesian(ylim=0:1) +
  #   xlab(paste0('Right Start ',d)) +
  #   ylab("Proportion of Choices Prioritizing Right Goal") +
  #   theme.goal +
  #   theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
  #   guides(color=guide_legend(title="Source"))

  #Trial Average (with SE)
  pd <- data_comb %>%
    group_by(x_factor,panel_factor,source,goal_type,subject) %>%
    summarise(choice_right = mean(prioritise_right),
              choice_right_upper = mean(prioritise_right_upper),
              choice_right_lower = mean(prioritise_right_lower)) %>%
    group_by(x_factor,panel_factor,source,goal_type) %>%
    summarise(choice_right = mean(choice_right),
              choice_right_upper = mean(choice_right_upper),
              choice_right_lower = mean(choice_right_lower),
              choice_se = sqrt(choice_right*(1-choice_right)/length(goal_type)))

  pd$choice_se[pd$source=="Predicted"]<-NA

  ggplot(data=pd,aes(x=x_factor)) +
    geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
    geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
    geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
    facet_grid(goal_type~panel_factor) +
    coord_cartesian(ylim=0:1) +
    xlab(paste0('Right Start ',d)) +
    ylab("Proportion of Choices Prioritizing Right Goal") +
    theme.goal +
    theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
    guides(color=guide_legend(title="Source"))

  ggsave(paste0(getwd(),"/PosteriorPredictives/TrialLevel-SE",d,"_",models[m],".pdf"),width=15,height=9)


  #Trial Average (with quantile)
  # pd <- data_comb %>%
  #   group_by(x_factor,panel_factor,source,goal_type,subject) %>%
  #   summarise(choice_right = mean(prioritise_right),
  #             choice_right_upper = mean(prioritise_right_upper),
  #             choice_right_lower = mean(prioritise_right_lower)) %>%
  #   group_by(x_factor,panel_factor,source,goal_type) %>%
  #   summarise(q10 = quantile(choice_right,0.1),
  #             q30 = quantile(choice_right,0.3),
  #             q50 = quantile(choice_right,0.5),
  #             q70 = quantile(choice_right,0.7),
  #             q90 = quantile(choice_right,0.9)) %>%
  #   tidyr::gather(key=quantile,value=choice_right,q10:q90) %>%
  #   mutate(quantile=factor(quantile,levels=c('q10','q30','q50','q70','q90'),labels=seq(0.1,0.9,by=0.2) ))
  #
  #
  # ggplot(data=pd,aes(x=x_factor)) +
  #   #geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  #   geom_line(aes(y=choice_right,group=interaction(quantile,factor(source)),color=factor(source))) +
  #   #geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
  #   facet_grid(goal_type~panel_factor) +
  #   coord_cartesian(ylim=0:1) +
  #   xlab(paste0('Right Start ',d)) +
  #   ylab("Proportion of Choices Prioritizing Right Goal") +
  #   theme.goal +
  #   theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
  #   guides(color=guide_legend(title="Source"))
  #
  # ggsave("TrialAverage-Quantile.pdf",width=15,height=12)
  #

  #Trial Average (by group)
  # pd <- data_comb %>%
  #   group_by(x_factor,panel_factor,source,goal_type,subject) %>%
  #   summarise(choice_right = mean(prioritise_right),
  #             choice_right_upper = mean(prioritise_right_upper),
  #             choice_right_lower = mean(prioritise_right_lower)) %>%
  #   mutate(strategy_group=factor(cut(choice_right,quantile(choice_right,probs=c(0,0.33,0.66,1))+c(-0.1,0,0,0.1)),labels=1:3)) %>%
  #   group_by(x_factor,panel_factor,source,goal_type,strategy_group) %>%
  #   summarise(choice_right = mean(choice_right))
  #
  # ggplot(data=pd,aes(x=x_factor)) +
  #   #geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  #   geom_line(aes(y=choice_right,group=interaction(strategy_group,factor(source)),color=factor(source))) +
  #   #geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
  #   facet_grid(goal_type~panel_factor) +
  #   coord_cartesian(ylim=0:1) +
  #   xlab(paste0('Right Start ',d)) +
  #   ylab("Proportion of Choices Prioritizing Right Goal") +
  #   theme.goal +
  #   theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
  #   guides(color=guide_legend(title="Source"))
  #
  # #ggsave("TrialAverage-Groups.pdf",width=15,height=12)
  # ggsave(paste0(getwd(),"/PosteriorPredictives/TrialLevel-Groups",d,"_",models[m],".pdf"),width=15,height=9)


  #ggsave(paste0(getwd(),"/PosteriorPredictives/TrialLevel",d,"_",models[m],".pdf"),width=15,height=9)

#}

}

}


# STAGE LEVEL PREDICTIVES -----

load("Models.Rda")

models.condition = list(Distance = c(1:20,61:63,67), #distance models
                        Deadline = c(1:60,64:66,68))

for(d in c("Distance",'Deadline')){

  data_obs = data %>% filter(right_current_distance>0.5,left_current_distance>0.5,phase==1,diffCon==d) %>%
    mutate(source = "Observed",
           prioritise_right_upper = NaN,
           prioritise_right_lower = NaN)

  data_pred = data_obs %>%
    mutate(source = "Predicted")

  for(m in models.condition[[d]]){
    print(m)
    for(f in c('Approach','Avoidance')){

      file=paste0(Sys.getenv('HOME'),'/Dropbox/Research/Projects/MGP-1517-GOAL/Modelling-5-RealDataHierarchical/Output/',d,f,'_',m,'_',models[m],'/Summary.Rda')
      if( file.exists(file) ){
        load(file)
        data_pred$prioritise_right[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"50%"]))
        data_pred$prioritise_right_upper[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"97.5%"]))
        data_pred$prioritise_right_lower[data_pred$goal_type==f] = 1/(1+exp(-smry[grep('p_a_logit',rownames(smry)),"2.5%"]))
      } else {
        data_pred$prioritise_right[data_pred$goal_type==f] = NA
        data_pred$prioritise_right_upper[data_pred$goal_type==f] = NA
        data_pred$prioritise_right_lower[data_pred$goal_type==f] = NA
      }
    }

    data_comb = bind_rows(data_obs,data_pred)

    if(d == "Distance"){
      data_comb$panel_factor = factor(data_comb$left_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Left Start: 30','Left Start: 45','Left Start: 67.5','Left Start: 90','Left Start: 112.5','Left Start: 135','Left Start: 150'))
      data_comb$x_factor = factor(data_comb$right_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Right Start: 30','Right Start: 45','Right Start: 67.5','Right Start: 90','Right Start: 112.5','Right Start: 135','Right Start: 150'))
         }
    if(d == "Deadline"){
      data_comb$panel_factor = factor(data_comb$left_start_deadline,levels=c(18,20,24,30,40,60,90),labels=c('Left Deadline: 18','Left Deadline: 20','Left Deadline: 24','Left Deadline: 30','Left Deadline: 40','Left Deadline: 60','Left Deadline: 90'))
      data_comb$x_factor = factor(data_comb$right_start_deadline,levels=c(18,20,24,30,40,60,90))
    }

    pd <- data_comb %>%
      mutate(stage_group = ceiling(stage/5)) %>%
      group_by(x_factor,panel_factor,source,goal_type,stage) %>%
      summarise(choice_right = mean(prioritise_right),
                choice_right_upper = mean(prioritise_right_upper),
                choice_right_lower = mean(prioritise_right_lower),
                choice_se = sqrt(choice_right*(1-choice_right)/length(stage)),
                nobs = length(stage)) %>% filter(nobs>9)
    pd$choice_se[pd$source=="Predicted"]<-NA

    ggplot(data=subset(pd,goal_type=="Avoidance"),aes(x=stage)) +
      geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
      geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
      geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
      facet_grid(panel_factor~x_factor) +
      coord_cartesian(ylim=0:1) +
      xlab(paste0('Right Start ',d)) +
      ylab("Proportion of Choices Prioritizing Right Goal") +
      theme.goal +
      theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
      guides(color=guide_legend(title="Source"))

    #ggsave(paste0(getwd(),"/PosteriorPredictives/",d,"_",models[m],".pdf"),width=15,height=9)

    #}

  }

}


pd <- data_comb %>%
  mutate(choice_far = prioritise_right*(left_current_distance<right_current_distance) +
           (1-prioritise_right)*(left_current_distance>right_current_distance),
         choice_far_upper = prioritise_right_upper*(left_current_distance<right_current_distance) +
           (1-prioritise_right_lower)*(left_current_distance>right_current_distance),
         choice_far_lower = prioritise_right_lower*(left_current_distance<right_current_distance) +
           (1-prioritise_right_upper)*(left_current_distance>right_current_distance),
         farther_start_distanceF = factor(farther_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Far: 30','Far: 45','Far: 67.5','Far: 90','Far: 112.5','Far: 135','Far: 150')),
         closer_start_distanceF = factor(closer_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Close: 30','Close: 45','Close: 67.5','Close: 90','Close: 112.5','Close: 135','Close: 150')))
pd$choice_far[pd$left_current_distance>=pd$right_current_distance]<-NA
pd$choice_far_upper[pd$left_current_distance>=pd$right_current_distance]<-NA
pd$choice_far_lower[pd$left_current_distance>=pd$right_current_distance]<-NA

pd <- pd %>%
  group_by(farther_start_distanceF,closer_start_distanceF,source,goal_type,stage) %>%
  summarise(choice_far = mean(choice_far,na.rm=T),
            choice_far_upper = mean(choice_far_upper,na.rm=T),
            choice_far_lower = mean(choice_far_lower,na.rm=T),
            choice_se = sqrt(choice_far*(1-choice_far)/length(stage)),
            nobs = length(stage)) %>% filter(nobs>9)
pd$choice_se[pd$source=="Predicted"]<-NA

ggplot(data=subset(pd,goal_type=="Avoidance"),aes(x=stage)) +
  geom_ribbon(aes(ymin=choice_far_lower,ymax=choice_far_upper,group=source),fill="lightblue") +
  geom_line(aes(y=choice_far,group=factor(source),color=factor(source))) +
  geom_errorbar(aes(ymin=choice_far-choice_se,ymax=choice_far+choice_se,group=factor(source),color=factor(source))) +
  facet_grid(farther_start_distanceF~closer_start_distanceF) +
  coord_cartesian(ylim=0:1,xlim=1:30) +
  xlab('Stage') +
  ylab("Proportion of Choices Prioritizing Farther Goal") +
  theme.goal +
  theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
  guides(color=guide_legend(title="Source")) +
  geom_hline(yintercept = 0.5,colour='gray')


ggsave("StageLevelPredictives-Avoidance.pdf",width=15,height=12)



#Traceplot of worst fitting conditions
#Editted by TB (29/11/2017)



data_obs$lik = (data_obs$prioritise_right==1)*data_pred$prioritise_right + (data_obs$prioritise_right==0)*(1-data_pred$prioritise_right)

sublik = data_obs %>% group_by(goal_type,subject) %>% summarise(mlik = mean(lik)) %>% group_by(goal_type) %>% mutate(rlik=rank(mlik))
trlik15030 = data_obs %>% filter(farther_start_distance==150,closer_start_distance==30) %>%  group_by(goal_type,subject) %>% summarise(trlik15030 = mean(lik)) %>% group_by(goal_type) %>% mutate(rtrlik15030=rank(trlik15030))
trlik = data_obs %>% group_by(subject,trial_number,goal_type) %>% summarise(trlik = mean(lik)) %>% group_by(goal_type) %>% mutate(rtrlik=rank(trlik))
lik = data_obs %>% select(subject,trial_number,stage,lik)

data_comb_lik = left_join(left_join(left_join(left_join(data_comb,sublik),trlik),lik),trlik15030)

bimodal=rep(FALSE,130)
bimodal[c(15,17,23,24,35,61,65,66,71,104,105,1,10,11,16,19,25,41,42,50,55,56,60,74,80,119,112,123,127,
          3,5,6,12,14,33,46,51,54,57,67,69,72,76,84,87,88,92,96,99,107,111,113,116,118)]<-TRUE

pd <- data_obs %>%
  mutate(sn = as.numeric(factor(subject)),
         bimodal=bimodal[sn],
          farther_start_distanceF = factor(farther_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Far: 30','Far: 45','Far: 67.5','Far: 90','Far: 112.5','Far: 135','Far: 150')),
         closer_start_distanceF = factor(closer_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Close: 30','Close: 45','Close: 67.5','Close: 90','Close: 112.5','Close: 135','Close: 150'))) %>%
  filter( goal_type=="Avoidance") %>%
  group_by(stage,farther_start_distanceF,closer_start_distanceF,bimodal,sn) %>%
  summarise(closer_start_distance_current = mean(closer_start_distance_current),
            farther_start_distance_current = mean(farther_start_distance_current),
            prioritise_farther = mean((farther_current_distance==left_current_distance)*(prioritise_right==0) +
                                        (farther_current_distance==right_current_distance)*(prioritise_right==1))) %>%
  mutate(crit_cond = 1*((closer_start_distanceF=='Close: 30')&(farther_start_distanceF=='Far: 150') ))

library(RColorBrewer)
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
p=ggplot(data=subset(pd),
       aes(y=farther_start_distance_current,x=closer_start_distance_current,
           group=interaction(farther_start_distanceF,closer_start_distanceF)))+
  geom_path(alpha=0.1)+
  geom_point(aes(colour=bimodal),size=0.5)+
  # geom_point(data=subset(pd,stage=1))+#aes(colour=factor(prioritise_farther)))+
  facet_wrap(~sn) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Farther Goal') +
  xlab("Distance to Closer Goal") +
  theme.goal #+ #theme(legend.position = 'none') +
  #scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

ggsave(file="Traceplots-BySubject-Avoidance-Bimodality.pdf",plot=p,height=20,width=20)



pd <- data_comb_lik %>%
  mutate(farther_start_distanceF = factor(farther_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Far: 30','Far: 45','Far: 67.5','Far: 90','Far: 112.5','Far: 135','Far: 150')),
         closer_start_distanceF = factor(closer_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Close: 30','Close: 45','Close: 67.5','Close: 90','Close: 112.5','Close: 135','Close: 150'))) %>%
  filter(rtrlik15030>100) %>%
  group_by(closer_start_distanceF,farther_start_distanceF,source,goal_type,stage) %>%
  summarise(choice_right = mean(prioritise_right),
            choice_right_upper = mean(prioritise_right_upper),
            choice_right_lower = mean(prioritise_right_lower),
            choice_se = sqrt(choice_right*(1-choice_right)/length(stage)),
            nobs = length(stage)) #%>% filter(nobs>9)
pd$choice_se[pd$source=="Predicted"]<-NA

ggplot(data=subset(pd,goal_type=="Approach"),aes(x=stage)) +
  geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
  geom_errorbar(aes(ymin=choice_right-choice_se,ymax=choice_right+choice_se,group=factor(source),color=factor(source))) +
  facet_grid(farther_start_distanceF~closer_start_distanceF) +
  coord_cartesian(ylim=0:1) +
  xlab(paste0('Right Start ',d)) +
  ylab("Proportion of Choices Prioritizing Right Goal") +
  theme.goal +
  theme(axis.text.x = element_text(size=8,colour="black",family="Times")) +
  guides(color=guide_legend(title="Source"))






ggplot(data=subset(pd),
       aes(y=farther_start_distance_current,x=closer_start_distance_current,
           group=rlik))+
  geom_path(alpha=0.1)+
  geom_point(aes(colour=lik),size=0.5)+
  # geom_point(data=subset(pd,stage=1))+#aes(colour=factor(prioritise_farther)))+
  facet_grid(farther_start_distanceF~closer_start_distanceF) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Farther Goal') +
  xlab("Distance to Closer Goal") +
  theme.goal + #theme(legend.position = 'none') +
  scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

ggsave(file="Traceplots-ByCondition-Approach.pdf",height=20,width=20)








#CHECK POSTERIOR PREDICTIVES - LINE PLOTS -----

pd <- data_comb %>%
  filter(goal_type=="Avoidance") %>%
  mutate(stage_group = factor(ceiling(stage/5),levels=1:6,labels=c('Stage 1-5','Stage 6-10','Stage 11-15','Stage 16-20','Stage 21-25','Stage 26-30')),
         left_start_distance_group = (left_start_distance<=45)*1 + ((left_start_distance>45)*(left_start_distance<135))*2 + (left_start_distance>=135)*3,
         right_start_distance_group = (right_start_distance<=45)*1 + ((right_start_distance>45)*(right_start_distance<135))*2 + (right_start_distance>=135)*3,
          left_start_distance_groupF = factor(left_start_distance_group,levels=1:3,labels=c('Left: Far','Left: Medium','Left: Close')),
          right_start_distance_groupF = factor(right_start_distance_group,levels=1:3,labels=c('Right: Far','Right: Medium','Right: Close')) ) %>%
  group_by(left_start_distance_groupF,right_start_distance_groupF,source,stage) %>%
  summarise(choice_right = mean(prioritise_right),
            choice_right_upper = mean(prioritise_right_upper),
            choice_right_lower = mean(prioritise_right_lower),
            choice_se = sqrt(choice_right*(1-choice_right)/length(stage)))

#pd$choice_right_upper[pd$source=='Observed']=pd$choice_right[pd$source=='Observed'] + pd$choice_se[pd$source=='Observed']
#pd$choice_right_lower[pd$source=='Observed']=pd$choice_right[pd$source=='Observed'] - pd$choice_se[pd$source=='Observed']


ggplot(data=pd,aes(x=stage)) +
  geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
  facet_grid(left_start_distance_groupF~right_start_distance_groupF) +
  coord_cartesian(xlim=1:30,ylim=0:1) +
  xlab('Stage') +
  ylab("Proportion of Choices Prioritizing Right Goal") +
  theme.goal

pd <- data_comb %>%
  #filter(goal_type=="Avoidance") %>%
  mutate(stage_group = factor(ceiling(stage/5),levels=1:6,labels=c('Stage 1-5','Stage 6-10','Stage 11-15','Stage 16-20','Stage 21-25','Stage 26-30')),
         left_start_distanceF = factor(left_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Left Start: 30','Left Start: 45','Left Start: 67.5','Left Start: 90','Left Start: 112.5','Left Start: 135','Left Start: 150')),
         right_start_distanceF = factor(right_start_distance,levels=c(30,45,67.5,90,112.5,135,150))) %>%
  group_by(left_start_distanceF,right_start_distanceF,source,goal_type) %>%
  summarise(choice_right = mean(prioritise_right),
            choice_right_upper = mean(prioritise_right_upper),
            choice_right_lower = mean(prioritise_right_lower),
            choice_se = sqrt(choice_right*(1-choice_right)/length(stage)))


ggplot(data=pd,aes(x=right_start_distanceF)) +
  geom_ribbon(aes(ymin=choice_right_lower,ymax=choice_right_upper,group=source),fill="lightblue") +
  geom_line(aes(y=choice_right,group=factor(source),color=factor(source))) +
  facet_grid(goal_type~left_start_distanceF) +
  coord_cartesian(ylim=0:1) +
  xlab('Right Start Distance') +
  ylab("Proportion of Choices Prioritizing Right Goal") +
  theme.goal +
  theme(axis.text.x = element_text(size=8,colour="black",family="Times"))

ggsave("PosteriorPredictives-FixedGradient.pdf",width=15,height=9)


ggplot(data=pd) +
  geom_line(aes(x=right_start_distance,y=choice_right,group=factor(left_start_distance),color=factor(left_start_distance))) +
  facet_grid(source~stage_group)


#HEAT MAPS OF CHOICE - DISTANCE -----

#data=data_bound
#tile plot - choice
pd <- data_bound %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(phase==1,expt==1) %>%
  mutate(left_current_distance_bin = round(left_current_distance/2)*2,
         right_current_distance_bin = round(right_current_distance/2)*2,
         stage_bin = factor(ceiling(stage/10),levels=1:3,labels=c('Stages 1-10','Stages 11-20','Stages 21-30')),
         prioritise_farther = (left_current_distance < right_current_distance)*prioritise_right +  (left_current_distance > right_current_distance)*(1-prioritise_right),
         policy_farther = (left_current_distance < right_current_distance)*policy +  (left_current_distance > right_current_distance)*(1-policy)) %>%
  group_by(left_current_distance_bin,right_current_distance_bin,goal_type) %>%
  summarise(count = length(stage),
            #right_prioritised = mean(prioritise_right),
            #policy_right = mean(policy),
            left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance),
            prioritise_farther = mean(prioritise_farther),
            policy_farther = mean(policy_farther) ) %>%
  filter(left_current_distance>-100,right_current_distance>-100) %>%
  gather(source,choice_farther,prioritise_farther,policy_farther)

pd$source = factor(pd$source,levels=c('prioritise_farther','policy_farther'),labels=c('Observed','Optimal'))

#pdp = bind_rows(pd,d_ap,d_av) #%>% filter(source=='Predicted')

ggplot(data=pd,aes(y=left_current_distance_bin,x=right_current_distance_bin,fill=choice_farther)) +
  geom_raster() +
  #facet_grid(left_start_distance~right_start_distance) +
  facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  coord_cartesian(ylim= c(-20,180),xlim=c(-20,180)) +
  ylab('Left Distance to Goal') +
  xlab("Right Distance to Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  labs(fill = "Proportion Prioritizing\nFarther Goal\n")

ggsave("HeatMap-Distance.pdf",width=8,height=9)

#HEAT MAPS OF CHOICE - DISTANCE - COLLAPSED ACROSS CHOICES WITHIN EACH CONDITION

#data=data_bound
#tile plot - choice
pd <- data_bound %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(phase==1,expt==1) %>%
  mutate( prioritise_farther = (left_current_distance < right_current_distance)*prioritise_right +  (left_current_distance > right_current_distance)*(1-prioritise_right) ,
  policy_farther = (left_current_distance < right_current_distance)*policy +  (left_current_distance > right_current_distance)*(1-policy)) %>%
  group_by(subject,trial_number,left_start_distance,right_start_distance,goal_type) %>%
  summarise( prioritise_farther = mean(prioritise_farther),
             policy_farther = mean(policy_farther)) %>%
  group_by(left_start_distance,right_start_distance,goal_type) %>%
  summarise(  prioritise_farther = mean(prioritise_farther),
            policy_farther = mean(policy_farther) ) %>%
  gather(source,choice_farther,prioritise_farther,policy_farther)

pd$source = factor(pd$source,levels=c('prioritise_farther','policy_farther'),labels=c('Observed','Optimal'))

#pdp = bind_rows(pd,d_ap,d_av) #%>% filter(source=='Predicted')

ggplot(data=pd,aes(y=factor(left_start_distance),x=factor(right_start_distance),fill=choice_farther)) +
  geom_raster() +
  #facet_grid(left_start_distance~right_start_distance) +
  facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
 # scale_x_reverse() +
#  scale_y_reverse() +
  #geom_vline(xintercept=0,size=2.5) +
  #geom_hline(yintercept=0,size=2.5) +
 # geom_vline(xintercept=0,size=0.5,color='yellow') +
  #geom_hline(yintercept=0,size=0.5,color='yellow') +
  #coord_cartesian(ylim= c(-20,180),xlim=c(-20,180)) +
  ylab('Left Distance to Goal at Start') +
  xlab("Right Distance to Goal at Start") +
  theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  scale_fill_distiller(palette="Spectral",limits=c(0,1)) +
  labs(fill = "Proportion Prioritizing\nFarther Goal\n")

ggsave("HeatMap-Distance.pdf",width=8,height=9)





#HEAT MAPS OF CHOICE - DEADLINE- COLLAPSED ACROSS CHOICES WITHIN EACH CONDITION

#data=data_bound
#tile plot - choice
pd <- data_bound %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(phase==1,expt==2) %>%
  mutate( prioritise_farther = (left_days_remaining < right_days_remaining)*prioritise_right +  (left_days_remaining > right_days_remaining)*(1-prioritise_right) ,
          policy_farther = (left_days_remaining < right_days_remaining)*policy +  (left_days_remaining > right_days_remaining)*(1-policy)) %>%
  group_by(subject,trial_number,left_start_deadline,right_start_deadline,goal_type) %>%
  summarise( prioritise_farther = mean(prioritise_farther),
             policy_farther = mean(policy_farther)) %>%
  group_by(left_start_deadline,right_start_deadline,goal_type) %>%
  summarise(  prioritise_farther = mean(prioritise_farther),
              policy_farther = mean(policy_farther) ) %>%
  gather(source,choice_farther,prioritise_farther,policy_farther)

pd$choice_farther[pd$left_start_deadline==pd$right_start_deadline] = NA

pd$source = factor(pd$source,levels=c('prioritise_farther','policy_farther'),labels=c('Observed','Optimal'))

#pdp = bind_rows(pd,d_ap,d_av) #%>% filter(source=='Predicted')

ggplot(data=pd,aes(y=factor(left_start_deadline),x=factor(right_start_deadline),fill=(1-choice_farther))) +
  geom_raster() +
  #facet_grid(left_start_distance~right_start_distance) +
  facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  # scale_x_reverse() +
  #  scale_y_reverse() +
  #geom_vline(xintercept=0,size=2.5) +
  #geom_hline(yintercept=0,size=2.5) +
  # geom_vline(xintercept=0,size=0.5,color='yellow') +
  #geom_hline(yintercept=0,size=0.5,color='yellow') +
  #coord_cartesian(ylim= c(-20,180),xlim=c(-20,180)) +
  ylab('Left Deadline at Start') +
  xlab("Right Deadline at Start") +
  theme.goal +
  #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
  scale_fill_distiller(palette="Spectral",limits=c(0,1)) +
  labs(fill = "Proportion Prioritizing\nCloser Deadline\n")

ggsave("HeatMap-Distance.pdf",width=8,height=9)





#HEAT MAPS OF CHOICE - DEADLINE -----


#data=data_bound
#tile plot - choice
pd_heat <- data_bound %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(phase==1,expt==2) %>%
  mutate(left_current_distance_bin = round(left_current_distance/2)*2,
         right_current_distance_bin = round(right_current_distance/2)*2,
         left_ToD = left_days_remaining/left_current_distance,
         right_ToD = right_days_remaining/right_current_distance,
         left_TAmTR = left_days_remaining - left_current_distance/3,
         right_TAmTR = right_days_remaining - right_current_distance/3,
         left_ToD_bin = round(left_ToD,-1),
         right_ToD_bin = round(right_ToD,-1),
         left_TAmTR_bin = round(left_TAmTR),
         right_TAmTR_bin = round(right_TAmTR),
         stage_bin = factor(ceiling(stage/10),levels=1:3,labels=c('Stages 1-10','Stages 11-20','Stages 21-30'))  ,
         prioritise_farther_dl = (left_days_remaining < right_days_remaining)*prioritise_right +  (left_days_remaining > right_days_remaining)*(1-prioritise_right),
         policy_farther_dl = (left_days_remaining < right_days_remaining)*policy +  (left_days_remaining > right_days_remaining)*(1-policy),
         left_start_deadline_bin = factor(as.numeric(left_start_deadline>=30),levels=0:1,labels=c('Left: Short','Left: Long')),
         right_start_deadline_bin = factor(as.numeric(right_start_deadline>=30),levels=0:1,labels=c('Right: Short','Right: Long'))) %>%
  group_by(left_start_deadline_bin,right_start_deadline_bin,goal_type,left_current_distance_bin,right_current_distance_bin) %>%
  summarise(count = length(stage),
            right_prioritised = mean(prioritise_right),
            policy_right = mean(policy),
            left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance),
            prioritise_farther_dl = mean(prioritise_farther_dl),
            policy_farther_dl = mean(policy_farther_dl) ) %>%
  filter(left_current_distance>-100,right_current_distance>-100) %>%
  gather(source,choice_farther,prioritise_farther_dl,policy_farther_dl)

pd_heat$source = factor(pd_heat$source,levels=c('prioritise_farther_dl','policy_farther_dl'),labels=c('Observed','Optimal'))

# pd_heat$left_start_deadlineF = factor(pd_heat$left_start_deadline,levels=c(18,20,24,30,40,60,90),
#                                 labels=c('Left: 18 Days','Left: 20 Days','Left: 24 Days','Left: 30 Days',
#                                          'Left: 40 Days','Left: 60 Days','Left: 90 Days'))
# pd_heat$right_start_deadlineF = factor(pd_heat$right_start_deadline,levels=c(18,20,24,30,40,60,90),
#                                 labels=c('Right: 18 Days','Right: 20 Days','Right: 24 Days','Right: 30 Days',
#                                          'Right: 40 Days','Right: 60 Days','Right: 90 Days'))


ggplot(data=subset(pd_heat,goal_type=='Approach'&source=='Observed'),
       aes(y=left_current_distance_bin,x=right_current_distance_bin,fill=(1-choice_farther))) +
  geom_tile() +
  facet_grid(left_start_deadline_bin~right_start_deadline_bin) +
  #facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  coord_cartesian(ylim= c(-20,100),xlim=c(-20,100)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  labs(fill = "Proportion Prioritizing\nShorter Deadline\n")

ggsave("HeatMap-Approach-Observed-Deadline.pdf",width=14,height=12)




ggplot(data=subset(pd_heat,goal_type=='Approach'&source=='Optimal'),
       aes(y=left_current_distance_bin,x=right_current_distance_bin,fill=choice_right)) +
  geom_tile() +
  facet_grid(left_start_deadlineF~right_start_deadlineF) +
  #facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  coord_cartesian(ylim= c(-20,100),xlim=c(-20,100)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  labs(fill = "Proportion Prioritizing\nRight Goal\n")

ggsave("HeatMap-Approach-Optimal-Deadline.pdf",width=14,height=12)

ggplot(data=subset(pd_heat,goal_type=='Avoidance'&source=='Observed'),
       aes(y=left_current_distance_bin,x=right_current_distance_bin,fill=choice_right)) +
  geom_tile() +
  facet_grid(left_start_deadlineF~right_start_deadlineF) +
  #facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  coord_cartesian(ylim= c(-20,100),xlim=c(-20,100)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  labs(fill = "Proportion Prioritizing\nRight Goal\n")

ggsave("HeatMap-Avoidance-Observed-Deadline.pdf",width=14,height=12)

ggplot(data=subset(pd_heat,goal_type=='Avoidance'&source=='Optimal'),
       aes(y=left_current_distance_bin,x=right_current_distance_bin,fill=choice_right)) +
  geom_tile() +
  facet_grid(left_start_deadlineF~right_start_deadlineF) +
  #facet_grid(source~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  geom_vline(xintercept=0,size=0.5,color='yellow') +
  geom_hline(yintercept=0,size=0.5,color='yellow') +
  coord_cartesian(ylim= c(-20,100),xlim=c(-20,100)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  labs(fill = "Proportion Prioritizing\nRight Goal\n")

ggsave("HeatMap-Avoidance-Optimal-Deadline.pdf",width=14,height=12)



#TRAJECTORY DISTRIBUTIONS - DISTANCE ------

#observed
#dividing up by quantile
pd_tmp <- data %>%
  filter(phase==1,left_current_distance>0,right_current_distance>0,diffCon=='Distance') %>%
  group_by(subject,trial_number,left_start_distance,right_start_distance,goal_type) %>%
  summarise(movement_right = mean(right_start_distance-min(right_current_distance)),
            movement_left = mean(left_start_distance-min(left_current_distance)),
            movement_diff = movement_right-movement_left,
            prop_right = mean(prioritise_right),
            movement_group = NaN)

gs = c('Approach','Avoidance')
ds = c(30,45,67.5,90,112.5,135,150)
for(g in gs){
  for(l_ds in ds){
    for(r_ds in ds){

      movement_diff = pd_tmp$movement_diff[which((pd_tmp$right_start_distance==r_ds)&
                                                   (pd_tmp$left_start_distance==l_ds)&
                                                   (pd_tmp$goal_type==g))]
      breaks = quantile(movement_diff,probs=c(0,0.1,0.3,0.4,0.6,0.7,0.9,1))        #seq(0,1,by=.1))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf

      pd_tmp[which((pd_tmp$right_start_distance==r_ds)&
                     (pd_tmp$left_start_distance==l_ds)&
                     (pd_tmp$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

    }
  }
}

pd_tmp1 = data %>% filter(diffCon=='Distance')
pd_tmp2 = pd_tmp %>% ungroup() %>% select(-goal_type,-left_start_distance,-right_start_distance)


pd_obs <- left_join(pd_tmp1,pd_tmp2,by=c('subject','trial_number')) %>%
  filter(movement_group == 2|movement_group == 4|movement_group == 6) %>%
  group_by(left_start_distance,right_start_distance,goal_type,movement_group,stage) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group,levels=c(2,4,6),labels=c('10-30 percentile','40-60 pecentile','70-90 percentile')))

# levels(pd_obs$movement_groupF) <- c('10-30','40-60','70-90')

ggplot(data=subset(pd_obs),
       aes(y=left_current_distance,x=right_current_distance,
           group=interaction(left_start_distance,right_start_distance),
           colour=interaction(left_start_distance,right_start_distance) )) +
  geom_point(data=subset(pd_obs,stage==1),size=2) +
  geom_path(alpha=0.6) +
  facet_grid(movement_groupF~goal_type) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal + theme(legend.position = 'none')

#optimal

#simulate optimal
load("DPOnly_StreamB.Rda")
full_dpdata30 = full_dpdata %>% filter(left_start_deadline==30,right_start_deadline==30)

sim_data = expand.grid(left_start_distance = c(30,45,67.5,90,112.5,135,150),
                       right_start_distance = c(30,45,67.5,90,112.5,135,150),
                       goal_type = c(0,1),
                       run = 1:1000,
                      stage = 1:30)

sim_data_mat = expand.grid(left_start_distance = c(30,45,67.5,90,112.5,135,150),
                       right_start_distance = c(30,45,67.5,90,112.5,135,150),
                       goal_type = c(0,1),
                       run = 1:1000)

nRuns = dim(sim_data_mat)[1]
left_current_distance = matrix(NaN,nRuns ,30)
right_current_distance = matrix(NaN,nRuns ,30)

left_current_distance[,1] = sim_data_mat$left_start_distance
right_current_distance[,1] = sim_data_mat$right_start_distance

bins = unique(full_dpdata$left_current_distance_bin)
total_bins = length(bins)


for(s in 1:29){
  left_current_distance_bin = round(left_current_distance[,s]/5)*5
  right_current_distance_bin = round(right_current_distance[,s]/5)*5

  policy_ind = sim_data_mat$goal_type*30*total_bins^2 +
               (s-1)*total_bins^2 +
               (match(left_current_distance_bin,bins)-1)*total_bins +
                match(right_current_distance_bin,bins)


  policy = 1-full_dpdata30$policy[policy_ind]

  random_choice = (runif(nRuns)>0.5)*1

  mean_left = ((policy==0)*(sim_data_mat$goal_type==0) + (policy==1)*(sim_data_mat$goal_type==1) + (policy==0.5)*(random_choice==0))*6
  mean_right = ((policy==0)*(sim_data_mat$goal_type==1) + (policy==1)*(sim_data_mat$goal_type==0) + (policy==0.5)*(random_choice==1))*6

  left_current_distance[,s+1]= pmax(left_current_distance[,s]    -  rnorm(nRuns,mean_left,3),-60) #policy 1 = right, 0 = left
  right_current_distance[,s+1] = pmax(right_current_distance[,s] -  rnorm(nRuns,mean_right,3),-60)

}

#calculate movement (have to do it this way because it takes ages in dplyr)
left_current_distance_greater_than_0 = left_current_distance
right_current_distance_greater_than_0 = right_current_distance
left_current_distance_greater_than_0[left_current_distance<0] = NaN
right_current_distance_greater_than_0[right_current_distance<0] = NaN
min_left_current_distance = apply(left_current_distance_greater_than_0,1,min,na.rm=T)
min_right_current_distance = apply(right_current_distance_greater_than_0,1,min,na.rm=T)
movement_left = sim_data_mat$left_start_distance - min_left_current_distance
movement_right = sim_data_mat$right_start_distance - min_right_current_distance
movement_diff = movement_right-movement_left

library(reshape2)
stage = t(array(1:30,dim=dim(t(left_current_distance))))
sim_data$stage2 = melt(stage)$value
sim_data$left_current_distance = melt(left_current_distance)$value
sim_data$right_current_distance = melt(right_current_distance)$value

sim_data_mat$movement_diff = movement_diff
pd_tmp_opt = sim_data_mat
pd_tmp_opt$movement_group = NaN

gs = 0:1
ds = c(30,45,67.5,90,112.5,135,150)
for(g in gs){
  for(l_ds in ds){
    for(r_ds in ds){

      movement_diff = pd_tmp_opt$movement_diff[which((pd_tmp_opt$right_start_distance==r_ds)&
                                                   (pd_tmp_opt$left_start_distance==l_ds)&
                                                   (pd_tmp_opt$goal_type==g))]
      breaks = quantile(movement_diff,probs=c(0,0.1,0.3,0.4,0.6,0.7,0.9,1))        #seq(0,1,by=.1))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf

      pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
                     (pd_tmp_opt$left_start_distance==l_ds)&
                     (pd_tmp_opt$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

    }
  }
}

#plot(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                    (pd_tmp_opt$left_start_distance==l_ds)&
#                    (pd_tmp_opt$goal_type==0)),'movement_group'],pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                                                                    (pd_tmp_opt$left_start_distance==l_ds)&
#                                                                                    (pd_tmp_opt$goal_type==0)),'movement_diff'])
#
# plot(density(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                 (pd_tmp_opt$left_start_distance==l_ds)&
#                                 (pd_tmp_opt$goal_type==0)),'movement_diff']))

#pd_tmp2 = pd_tmp_opt %>% ungroup() %>% select(-goal_type,-left_start_distance,-right_start_distance)
pd_opt <- left_join(sim_data,pd_tmp_opt,by=c('run','goal_type','left_start_distance','right_start_distance')) %>%
  filter(movement_group == 2|movement_group == 4|movement_group == 6) %>%
  group_by(left_start_distance,right_start_distance,goal_type,movement_group,stage) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group,levels=c(2,4,6),labels=c('10-30 percentile','40-60 pecentile','70-90 percentile')))


pd_opt$goal_type <- factor(pd_opt$goal_type,levels=0:1,labels=c('Approach','Avoidance'))



#simulate predicted

#full_dpdata30 = full_dpdata %>% filter(left_start_deadline==30,right_start_deadline==30)
library(truncnorm)

sim_data = expand.grid(left_start_distance = c(30,45,67.5,90,112.5,135,150),
                       right_start_distance = c(30,45,67.5,90,112.5,135,150),
                       goal_type = c(0,1),
                       run = 1:1000,
                       stage = 1:30)


sim_data_mat = expand.grid(left_start_distance = c(30,45,67.5,90,112.5,135,150),
                           right_start_distance = c(30,45,67.5,90,112.5,135,150),
                           goal_type = c(0,1),
                           run = 1:1000)

#set parms
sim_data_mat$w1_mean = NaN
sim_data_mat$w1_mean[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["w1_mean","mean"],sd=ap_parms["w1_mean","sd"])
sim_data_mat$w1_mean[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["w1_mean","mean"],sd=av_parms["w1_mean","sd"])
sim_data_mat$w1_sd = NaN
sim_data_mat$w1_sd[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["w1_sd","mean"],sd=ap_parms["w1_sd","sd"])
sim_data_mat$w1_sd[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["w1_sd","mean"],sd=av_parms["w1_sd","sd"])
sim_data_mat$w1 = rtruncnorm(n=dim(sim_data_mat)[1],a=0,b=Inf,mean=sim_data_mat$w1_mean,sd=sim_data_mat$w1_sd)

sim_data_mat$delta = -1.5

#sim_data_mat$delta_mean = NaN
#sim_data_mat$delta_mean[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["delta_mean","mean"],sd=ap_parms["delta_mean","sd"])
#sim_data_mat$delta_mean[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["delta_mean","mean"],sd=av_parms["delta_mean","sd"])
#sim_data_mat$delta_sd = NaN
#sim_data_mat$delta_sd[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["delta_sd","mean"],sd=ap_parms["delta_sd","sd"])
#sim_data_mat$delta_sd[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["delta_sd","mean"],sd=av_parms["delta_sd","sd"])
#sim_data_mat$delta = -rtruncnorm(n=dim(sim_data_mat)[1],a=0,b=Inf,mean=sim_data_mat$delta_mean,sd=sim_data_mat$w1_sd)

sim_data_mat$w3_mean = NaN
sim_data_mat$w3_mean[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["w3_mean","mean"],sd=ap_parms["w3_mean","sd"])
sim_data_mat$w3_mean[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["w3_mean","mean"],sd=av_parms["w3_mean","sd"])
sim_data_mat$w3_sd = NaN
sim_data_mat$w3_sd[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["w3_sd","mean"],sd=ap_parms["w3_sd","sd"])
sim_data_mat$w3_sd[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["w3_sd","mean"],sd=av_parms["w3_sd","sd"])
sim_data_mat$w3 = rtruncnorm(n=dim(sim_data_mat)[1],a=0,b=Inf,mean=sim_data_mat$w3_mean,sd=sim_data_mat$w3_sd)

sim_data_mat$alpha_a = NaN
sim_data_mat$alpha_a[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["alpha_a","mean"],sd=ap_parms["alpha_a","sd"])
sim_data_mat$alpha_a[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["alpha_a","mean"],sd=av_parms["alpha_a","sd"])
sim_data_mat$alpha_b = NaN
sim_data_mat$alpha_b[sim_data_mat$goal_type==0] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=ap_parms["alpha_b","mean"],sd=ap_parms["alpha_b","sd"])
sim_data_mat$alpha_b[sim_data_mat$goal_type==1] <- rtruncnorm(n=dim(sim_data_mat)[1]/2,a=0,b=Inf,mean=av_parms["alpha_b","mean"],sd=av_parms["alpha_b","sd"])
sim_data_mat$alpha = rbeta(n=dim(sim_data_mat)[1],shape1=sim_data_mat$alpha_a,shape2=sim_data_mat$alpha_b)

sim_data_mat$max_spatiotemporal =  sim_data_mat$alpha / sqrt(sim_data_mat$alpha/(1-sim_data_mat$alpha)) + (1-sim_data_mat$alpha)*sqrt(sim_data_mat$alpha/(1-sim_data_mat$alpha))

nRuns = dim(sim_data_mat)[1]
left_current_distance = matrix(NaN,nRuns ,30)
right_current_distance = matrix(NaN,nRuns ,30)

left_current_distance[,1] = sim_data_mat$left_start_distance
right_current_distance[,1] = sim_data_mat$right_start_distance

# bins = unique(full_dpdata$left_current_distance_bin)
# total_bins = length(bins)


for(s in 1:29){

  m_r = sim_data_mat$w1*right_current_distance[,s]^sim_data_mat$delta +
        sim_data_mat$w3*sim_data_mat$max_spatiotemporal /
          (sim_data_mat$alpha*((30-s)/right_current_distance[,s]) +
             sim_data_mat$alpha*(right_current_distance[,s]/(30-s)))

  m_r[right_current_distance[,s]<0] <- 0 #no motivation once goal is achieved

  m_l = sim_data_mat$w1*left_current_distance[,s]^sim_data_mat$delta +
    sim_data_mat$w3*sim_data_mat$max_spatiotemporal /
    (sim_data_mat$alpha*((30-s)/left_current_distance[,s]) +
       sim_data_mat$alpha*(left_current_distance[,s]/(30-s)))
  m_l[left_current_distance[,s]<0] <- 0 #no motivation once goal is achieved

  policy = 1/(1+exp(-(m_r-m_l)))

  random_choice = (runif(nRuns)<policy)*1

  mean_left = (sim_data_mat$goal_type==0)*(random_choice==0)*6 + (sim_data_mat$goal_type==1)*(random_choice==1)*6
  mean_right =(sim_data_mat$goal_type==0)*(random_choice==1)*6 + (sim_data_mat$goal_type==1)*(random_choice==0)*6

  left_current_distance[,s+1]= pmax(left_current_distance[,s]    -  rnorm(nRuns,mean_left,3),-60) #policy 1 = right, 0 = left
  right_current_distance[,s+1] = pmax(right_current_distance[,s] -  rnorm(nRuns,mean_right,3),-60)

}

#calculate movement (have to do it this way because it takes ages in dplyr)
left_current_distance_greater_than_0 = left_current_distance
right_current_distance_greater_than_0 = right_current_distance
left_current_distance_greater_than_0[left_current_distance<0] = NaN
right_current_distance_greater_than_0[right_current_distance<0] = NaN
min_left_current_distance = apply(left_current_distance_greater_than_0,1,min,na.rm=T)
min_right_current_distance = apply(right_current_distance_greater_than_0,1,min,na.rm=T)
movement_left = sim_data_mat$left_start_distance - min_left_current_distance
movement_right = sim_data_mat$right_start_distance - min_right_current_distance
movement_diff = movement_right-movement_left

library(reshape2)
stage = t(array(1:30,dim=dim(t(left_current_distance))))
sim_data$stage2 = melt(stage)$value
sim_data$left_current_distance = melt(left_current_distance)$value
sim_data$right_current_distance = melt(right_current_distance)$value

sim_data_mat$movement_diff = movement_diff
pd_tmp_pred = sim_data_mat
pd_tmp_pred$movement_group = NaN

gs = 0:1
ds = c(30,45,67.5,90,112.5,135,150)
for(g in gs){
  for(l_ds in ds){
    for(r_ds in ds){

      movement_diff = pd_tmp_pred$movement_diff[which((pd_tmp_pred$right_start_distance==r_ds)&
                                                       (pd_tmp_pred$left_start_distance==l_ds)&
                                                       (pd_tmp_pred$goal_type==g))]
      breaks = quantile(movement_diff,probs=c(0,0.1,0.3,0.4,0.6,0.7,0.9,1))        #seq(0,1,by=.1))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf

      pd_tmp_pred[which((pd_tmp_pred$right_start_distance==r_ds)&
                         (pd_tmp_pred$left_start_distance==l_ds)&
                         (pd_tmp_pred$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

    }
  }
}

#plot(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                    (pd_tmp_opt$left_start_distance==l_ds)&
#                    (pd_tmp_opt$goal_type==0)),'movement_group'],pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                                                                    (pd_tmp_opt$left_start_distance==l_ds)&
#                                                                                    (pd_tmp_opt$goal_type==0)),'movement_diff'])
#
# plot(density(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                 (pd_tmp_opt$left_start_distance==l_ds)&
#                                 (pd_tmp_opt$goal_type==0)),'movement_diff']))

#pd_tmp2 = pd_tmp_opt %>% ungroup() %>% select(-goal_type,-left_start_distance,-right_start_distance)
pd_pred <- left_join(sim_data,pd_tmp_pred,by=c('run','goal_type','left_start_distance','right_start_distance')) %>%
  filter(movement_group == 2|movement_group == 4|movement_group == 6) %>%
  group_by(left_start_distance,right_start_distance,goal_type,movement_group,stage) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group,levels=c(2,4,6),labels=c('10-30 percentile','40-60 pecentile','70-90 percentile')))


pd_pred$goal_type <- factor(pd_pred$goal_type,levels=0:1,labels=c('Approach','Avoidance'))













pd_obs$source = 'Observed'
pd_opt$source = 'Optimal'
pd_pred$source = 'Predicted'
#levels(pd_opt$movement_groupF) <- c('10-30 percentile','40-60 pecentile','70-90 percentile')
#levels(pd_obs$movement_groupF) <- c('10-30 percentile','40-60 pecentile','70-90 percentile')
#levels(pd_pred$movement_groupF) <- c('10-30 percentile','40-60 pecentile','70-90 percentile')
pd = rbind(pd_obs,pd_opt,pd_pred)

#Note, check to see if sharp turn at -60 as an artefact of the statespace (that it was bounded at -60)

ggplot(data=subset(pd),
       aes(y=left_current_distance,x=right_current_distance,
           group=interaction(left_start_distance,right_start_distance),
           colour=interaction(left_start_distance,right_start_distance) )) +
  geom_point(data=subset(pd,stage==1),size=2) +
  geom_path(alpha=0.6) +
  facet_grid(movement_groupF~source+goal_type) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  theme.goal + theme(legend.position = 'none')

ggsave("TrajectoryDistribution_1030_4060_7090_ObsOptPred-Distance-FixedSpatial-Delta=-1.5.pdf",width=14,height=10)












#TRAJECTORY DISTRIBUTIONS - DEADLINE ------

#observed
#dividing up by quantile
pd_tmp <- data %>%
  filter(phase==1,left_current_distance>0,right_current_distance>0,diffCon=='Deadline') %>%
  group_by(subject,trial_number,left_start_deadline,right_start_deadline,goal_type) %>%
  summarise(movement_right = mean(right_start_distance-min(right_current_distance)),
            movement_left = mean(left_start_distance-min(left_current_distance)),
            movement_diff = movement_right-movement_left,
            prop_right = mean(prioritise_right),
            movement_group = NaN)

gs = c('Approach','Avoidance')
ds = c(18,20,24,30,40,60,90)
for(g in gs){
  for(l_ds in ds){
    for(r_ds in ds){

      movement_diff = pd_tmp$movement_diff[which((pd_tmp$right_start_deadline==r_ds)&
                                                   (pd_tmp$left_start_deadline==l_ds)&
                                                   (pd_tmp$goal_type==g))]
      breaks = quantile(movement_diff,probs=c(0,0.1,0.3,0.4,0.6,0.7,0.9,1))        #seq(0,1,by=.1))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf

      pd_tmp[which((pd_tmp$right_start_deadline==r_ds)&
                     (pd_tmp$left_start_deadline==l_ds)&
                     (pd_tmp$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

    }
  }
}

pd_tmp1 = data %>% filter(diffCon=='Deadline',phase==1)
pd_tmp2 = pd_tmp %>% ungroup() %>% select(-goal_type,-left_start_deadline,-right_start_deadline)


pd_obs <- left_join(pd_tmp1,pd_tmp2,by=c('subject','trial_number')) %>%
  filter(movement_group == 2|movement_group == 4|movement_group == 6) %>%
  group_by(left_start_deadline,right_start_deadline,goal_type,movement_group,stage) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group ))

levels(pd_obs$movement_groupF) <- c('10-30','40-60','70-90')

#optimal

#simulate optimal
load("DPOnly_StreamB.Rda")

# dpdata = data %>% filter(diffCon=='Deadline',phase==1) %>%
#   mutate( left_current_distance_bin = round(left_current_distance/5)*5,
#           right_current_distance_bin = round(right_current_distance/5)*5 ) %>%
#   group_by(goal_type,stage,left_start_deadline,right_start_deadline,left_current_distance_bin,right_current_distance_bin) %>%
#   summarise(policy = mean(policy))

deadlines = c(18,20,24,30,40,60,90)

sim_data = expand.grid(left_start_deadline = c(18,20,24,30,40,60,90),
                       right_start_deadline = c(18,20,24,30,40,60,90),
                       goal_type = c(0,1),
                       run = 1:1000,
                      stage = 1:90)

sim_data_mat_tmp = expand.grid(left_start_deadline= c(18,20,24,30,40,60,90),
                       right_start_deadline = c(18,20,24,30,40,60,90),
                       goal_type = c(0,1),
                       run = 1:1000)





nRuns = dim(sim_data_mat_tmp)[1]
left_current_distance = matrix(NaN,nRuns ,90)
right_current_distance = matrix(NaN,nRuns ,90)

left_current_distance[,1] = 90
right_current_distance[,1] = 90

bins = unique(full_dpdata$left_current_distance_bin)
total_bins = length(bins)

nObs = expand.grid(goal_type = c(0,1),
                   left_start_deadline= c(18,20,24,30,40,60,90),
                   right_start_deadline = c(18,20,24,30,40,60,90)) %>%
  mutate(stages = pmax(left_start_deadline,right_start_deadline),
         nObsPerStage = total_bins^2,
         nObsPrior = lag(cumsum(nObsPerStage*stages)))
nObs$nObsPrior[1] <- 0

sim_data_mat = left_join(sim_data_mat_tmp,nObs,by = c("left_start_deadline", "right_start_deadline", "goal_type"))

for(s in 1:89){
  left_current_distance_bin = round(left_current_distance[,s]/5)*5
  right_current_distance_bin = round(right_current_distance[,s]/5)*5

  policy_ind = sim_data_mat$nObsPrior +
                (s<=sim_data_mat$stages)*((s-1)*total_bins^2) +
                (s>sim_data_mat$stages)*((sim_data_mat$stages-1)*total_bins^2) +
               (match(left_current_distance_bin,bins)-1)*total_bins +
                match(right_current_distance_bin,bins)


  sum(left_current_distance_bin!=full_dpdata$left_current_distance_bin[policy_ind])
  sum(right_current_distance_bin!=full_dpdata$right_current_distance_bin[policy_ind])
  sum(sim_data_mat$left_start_deadline!=full_dpdata$left_start_deadline[policy_ind])
  sum(sim_data_mat$right_start_deadline!=full_dpdata$right_start_deadline[policy_ind])
  sum((sim_data_mat$goal_type+1)!=full_dpdata$frame[policy_ind])
  sum(s!=full_dpdata$stage[policy_ind])

  trial_terminated = sim_data_mat$stages<s

  policy = 1-full_dpdata$policy[policy_ind]

  random_choice = (runif(nRuns)>0.5)*1

  mean_left = ((policy==0)*(sim_data_mat$goal_type==0) + (policy==1)*(sim_data_mat$goal_type==1) + (policy==0.5)*(random_choice==0))*6
  mean_right = ((policy==0)*(sim_data_mat$goal_type==1) + (policy==1)*(sim_data_mat$goal_type==0) + (policy==0.5)*(random_choice==1))*6

  left_current_distance[,s+1]=   pmax(left_current_distance[,s]  -      (1-trial_terminated)*rnorm(nRuns,mean_left,3),       -60) #policy 1 = right, 0 = left
  right_current_distance[,s+1] = pmax(right_current_distance[,s] -      (1-trial_terminated)*rnorm(nRuns,mean_right,3),      -60)

}

#calculate movement (have to do it this way because it takes ages in dplyr)
left_current_distance_greater_than_0 = left_current_distance
right_current_distance_greater_than_0 = right_current_distance
left_current_distance_greater_than_0[left_current_distance<0] = NaN
right_current_distance_greater_than_0[right_current_distance<0] = NaN
min_left_current_distance = apply(left_current_distance_greater_than_0,1,min,na.rm=T)
min_right_current_distance = apply(right_current_distance_greater_than_0,1,min,na.rm=T)
movement_left = 90 - min_left_current_distance
movement_right = 90 - min_right_current_distance
movement_diff = movement_right-movement_left

library(reshape2)
stage = t(array(1:90,dim=dim(t(left_current_distance))))
sim_data$stage2 = melt(stage)$value
sim_data$left_current_distance = melt(left_current_distance)$value
sim_data$right_current_distance = melt(right_current_distance)$value

sim_data_mat$movement_diff = movement_diff
pd_tmp_opt = sim_data_mat
pd_tmp_opt$movement_group = NaN

gs = 0:1
ds = c(18,20,24,30,40,60,90)
for(g in gs){
  for(l_ds in ds){
    for(r_ds in ds){

      movement_diff = pd_tmp_opt$movement_diff[which((pd_tmp_opt$right_start_deadline==r_ds)&
                                                   (pd_tmp_opt$left_start_deadline==l_ds)&
                                                   (pd_tmp_opt$goal_type==g))]
      breaks = quantile(movement_diff,probs=c(0,0.1,0.3,0.4,0.6,0.7,0.9,1))        #seq(0,1,by=.1))
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf

      pd_tmp_opt[which((pd_tmp_opt$right_start_deadline==r_ds)&
                     (pd_tmp_opt$left_start_deadline==l_ds)&
                     (pd_tmp_opt$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

    }
  }
}

#plot(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                    (pd_tmp_opt$left_start_distance==l_ds)&
#                    (pd_tmp_opt$goal_type==0)),'movement_group'],pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                                                                    (pd_tmp_opt$left_start_distance==l_ds)&
#                                                                                    (pd_tmp_opt$goal_type==0)),'movement_diff'])
#
# plot(density(pd_tmp_opt[which((pd_tmp_opt$right_start_distance==r_ds)&
#                                 (pd_tmp_opt$left_start_distance==l_ds)&
#                                 (pd_tmp_opt$goal_type==0)),'movement_diff']))

#pd_tmp2 = pd_tmp_opt %>% ungroup() %>% select(-goal_type,-left_start_distance,-right_start_distance)
pd_opt <- left_join(sim_data,pd_tmp_opt,by=c('run','goal_type','left_start_deadline','right_start_deadline')) %>%
  filter(movement_group == 2|movement_group == 4|movement_group == 6) %>%
  group_by(left_start_deadline,right_start_deadline,goal_type,movement_group,stage) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group ))


pd_opt$goal_type <- factor(pd_opt$goal_type,levels=0:1,labels=c('Approach','Avoidance'))

pd_obs$source = 'Observed'
pd_opt$source = 'Optimal'
levels(pd_opt$movement_groupF) <- c('10-30 percentile','40-60 pecentile','70-90 percentile')
levels(pd_obs$movement_groupF) <- c('10-30 percentile','40-60 pecentile','70-90 percentile')
pd = rbind(pd_obs,pd_opt)
pd$left_start_deadlineF = factor(pd$left_start_deadline,levels=c(18,20,24,30,40,60,90),
                                 labels=c('Left: 18 Days','Left: 20 Days','Left: 24 Days','Left: 30 Days',
                                          'Left: 40 Days','Left: 60 Days','Left: 90 Days'))
pd$right_start_deadlineF = factor(pd$right_start_deadline,levels=c(18,20,24,30,40,60,90),
                                  labels=c('Right: 18 Days','Right: 20 Days','Right: 24 Days','Right: 30 Days',
                                           'Right: 40 Days','Right: 60 Days','Right: 90 Days'))
pd$left_current_distance[pd$stage>pmin(pd$left_start_deadline,pd$right_start_deadline)] <- NA
#Note, check to see if sharp turn at -60 as an artefact of the statespace (that it was bounded at -60)

ggplot(data=subset(pd,goal_type=='Approach'),
       aes(y=left_current_distance,x=right_current_distance,
           group=interaction(movement_groupF,source),
           colour=movement_groupF,
           linetype=source)) +
  #geom_tile(data=subset(pd_heat,source=='Observed'&goal_type=='Approach'),
  #           aes(y=left_current_distance_bin,x=right_current_distance_bin,group=NA,colour=NA,linetype=NA,fill=choice_right)) +
  geom_point(data=subset(pd_obs,stage==1),size=2,colour='black') +
  geom_path() +
  facet_grid(left_start_deadlineF~right_start_deadlineF) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(fill = "Proportion Prioritizing\nRight Goal\n",linetype="Source",colour="Group") +
  theme.goal
ggsave("TrajectoryDistribution_Approach-Deadline.pdf",width=14,height=12)


ggplot(data=subset(pd,goal_type=='Avoidance'),
       aes(y=left_current_distance,x=right_current_distance,
           group=interaction(movement_groupF,source),
           colour=movement_groupF,
           linetype=source)) +
  #geom_tile(data=subset(pd_heat,source=='Observed'&goal_type=='Approach'),
  #           aes(y=left_current_distance_bin,x=right_current_distance_bin,group=NA,colour=NA,linetype=NA,fill=choice_right)) +
  geom_point(data=subset(pd_obs,stage==1),size=2,colour='black') +
  geom_path() +
  facet_grid(left_start_deadlineF~right_start_deadlineF) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-60,160),xlim=c(-60,160)) +
  ylab('Distance to Left Goal') +
  xlab("Distance to Right Goal") +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(fill = "Proportion Prioritizing\nRight Goal\n",linetype="Source",colour="Group") +
  theme.goal #+
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  # theme(panel.margin = unit(1, "lines"),
  #       strip.text.x = element_text(size=12,face="bold",colour="black",family="Times"),
  #       strip.text.y = element_text(size=12, face="bold","black",family="Times"),
  #       axis.text.x = element_text(size=12,colour="black",family="Times"),
  #       axis.text.y = element_text(size=12,colour="black",family="Times"),
  #       legend.text=element_text(size=12),
  #       legend.title =element_text(size=14))#,
  #      # legend.position = 'none')

ggsave("TrajectoryDistribution_Avoidance-Deadline.pdf",width=14,height=12)









#OTHER -----




pd <- data %>%
  filter(phase==1,diffCon=="Distance",goal_type=="Approach") %>%
  mutate(left_current_distance_bin = round(left_current_distance/2)*2,
         right_current_distance_bin = round(right_current_distance/2)*2,
         stage_bin = factor(ceiling(stage/10),levels=1:3,labels=c('Stages 1-10','Stages 11-20','Stages 21-30'))  ) %>%
  group_by(left_current_distance_bin,right_current_distance_bin,goal_type,stage) %>%
  summarise(count = length(stage),
            right_prioritised = mean(prioritise_right),
            policy_right = mean(policy_right),
            left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance))

ggplot(data=pd,aes(x=left_current_distance_bin,y=right_current_distance_bin,fill=policy_right)) +
  geom_tile() +
  #facet_grid(left_start_distance~right_start_distance) +
  facet_wrap(~stage) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0,size=2.5) +
  geom_hline(yintercept=0,size=2.5) +
  coord_cartesian(ylim= c(-20,180),xlim=c(-20,180)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black') +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.spacing = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))
#legend.position = 'none')






pd <- data %>%
  filter(left_current_distance>0,right_current_distance>0,phase==1) %>%
  group_by(goal_type,diffCon,left_difficultyF,right_difficultyF) %>%
  summarise(obs_choice = mean(prioritise_right),
            opt_choice = mean(policy_right,na.rm=T),
            left_difficulty = mean(left_difficulty),
            right_difficulty = mean(right_difficulty))


# pd <- data %>%
#   group_by(goal_type,left_current_distance_bin,right_current_distance_bin,left_start_deadline,right_start_deadline) %>%
#   summarise(obs_choice = mean(prioritise_right),
#             opt_choice = mean(policy_right,na.rm=T),
#             left_difficulty = mean(left_difficulty),
#             right_difficulty = mean(right_difficulty))



ggplot(data=pd,aes(x=left_difficultyF,y=right_difficultyF,z=obs_choice)) +
  facet_wrap(diffCon~goal_type) +
  geom_tile(aes(fill=obs_choice)) +
  #scale_fill_continuous(limits=c(.1, .9), breaks=seq(.1,9,by=0.2))
  geom_contour(aes(x=left_difficulty,y=right_difficulty),colour="black",binwidth=0.1,size=.2) +
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0, 1),breaks=seq(0,1,by=0.25),name="Proportion\nPrioritising\nRight Goal") +
  coord_cartesian(ylim= c(1,7),xlim=c(1,7)) +
  xlab('Left Goal Difficulty') +
  ylab("Right Goal Difficulty") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))

ggsave("ObservedAverageChoice.pdf",width=6,height=5)


ggplot(data=pd,aes(x=left_difficultyF,y=right_difficultyF,z=opt_choice)) +
  facet_wrap(diffCon~goal_type) +
  geom_tile(aes(fill=opt_choice)) +
  #scale_fill_continuous(limits=c(.1, .9), breaks=seq(.1,9,by=0.2))
  geom_contour(aes(x=left_difficulty,y=right_difficulty),colour="black",binwidth=0.1,size=.2) +
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0, 1),breaks=seq(0,1,by=0.25),name="Proportion\nPrioritising\nRight Goal") +
  coord_cartesian(ylim= c(1,7),xlim=c(1,7)) +
  xlab('Left Goal Difficulty') +
  ylab("Right Goal Difficulty") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))

ggsave("OptimalAverageChoice.pdf",width=6,height=5)






pd <- data %>%
  filter(left_current_distance>0,right_current_distance>0,stage==1) %>%
  group_by(goal_type,diffCon,left_difficultyF,right_difficultyF) %>%
  summarise(obs_choice = mean(prioritise_right),
            opt_choice = mean(policy_right,na.rm=T),
            left_difficulty = mean(left_difficulty),
            right_difficulty = mean(right_difficulty))


ggplot(data=pd,aes(x=left_difficultyF,y=right_difficultyF,z=obs_choice)) +
  facet_wrap(diffCon~goal_type) +
  geom_tile(aes(fill=obs_choice)) +
  #scale_fill_continuous(limits=c(.1, .9), breaks=seq(.1,9,by=0.2))
  geom_contour(aes(x=left_difficulty,y=right_difficulty),colour="black",binwidth=0.1,size=.2) +
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0, 1),breaks=seq(0,1,by=0.25),name="Proportion\nPrioritising\nRight Goal") +
  coord_cartesian(ylim= c(1,7),xlim=c(1,7)) +
  xlab('Left Goal Difficulty') +
  ylab("Right Goal Difficulty") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))

ggsave("ObservedAverageFirstChoice.pdf",width=6,height=5)


ggplot(data=pd,aes(x=left_difficultyF,y=right_difficultyF,z=opt_choice)) +
  facet_wrap(diffCon~goal_type) +
  geom_tile(aes(fill=opt_choice)) +
  #scale_fill_continuous(limits=c(.1, .9), breaks=seq(.1,9,by=0.2))
  geom_contour(aes(x=left_difficulty,y=right_difficulty),colour="black",binwidth=0.1,size=.2) +
  scale_fill_distiller(palette="Spectral", na.value="white",limits=c(0, 1),breaks=seq(0,1,by=0.25),name="Proportion\nPrioritising\nRight Goal") +
  coord_cartesian(ylim= c(1,7),xlim=c(1,7)) +
  xlab('Left Goal Difficulty') +
  ylab("Right Goal Difficulty") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))

ggsave("OptimalAverageFirstChoice.pdf",width=6,height=5)


#Trajectory Plots

pd <- data %>%
  filter(phase==1) %>%
  group_by(goal_type,left_difficulty,right_difficulty,stage,diffCon) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance),
            obs_choice = mean(prioritise_right),
            opt_choice = mean(policy_right))

# ggplot(data=subset(pd,goal_type=="Approach"),aes(x=left_current_distance,y=right_current_distance)) +
#   geom_path() +
#   facet_grid(left_start_distance~right_start_distance) +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   geom_vline(xintercept=0) +
#   geom_hline(yintercept=0) +
#   theme.goal +
#   theme(panel.margin = unit(1, "lines"),
#         strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
#         strip.text.y = element_text(size=8, face="bold","black",family="Times"),
#         axis.text.x = element_text(size=6,colour="black",family="Times"),
#         axis.text.y = element_text(size=6,colour="black",family="Times"))

#trajectory plots that are transparent
pd <- data %>%
  filter(phase==1,diffCon=="Distance",goal_type=="Approach") %>%
  group_by(left_difficulty,right_difficulty,stage,subject) %>%
  summarise(left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance),
            obs_choice = mean(prioritise_right),
            opt_choice = mean(policy_right))

ggplot(data=pd,aes(x=left_current_distance,y=right_current_distance,
                                                 group=subject,
                                                 colour=subject)) +
  geom_path(alpha=0.1) +
  facet_grid(left_difficulty~right_difficulty) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')

ggsave("AveragePhaseSpaceTrajectory.pdf",width=6,height=5)

#points
ggplot(data=pd,aes(x=left_current_distance,y=right_current_distance,
                   group=subject,
                   colour=subject)) +
  geom_point(alpha=0.1) +
  facet_grid(left_difficulty~right_difficulty) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')


#WHEEL PLOT - the number of points that fall in each slice
bin_size = 10
bins = seq(300,-300,by=-bin_size)

project_crossing <- function(x0,y0,x,y){
  #print(c(x,y))
  slope = (y-y0)/(x-x0)
  p = NA
  if(!is.na(slope)){
    if(abs(slope)>1){
      p = (x - y/slope)*-1 #if y is winning
    }
    if(abs(slope)<1){
      p = y - slope*x #if x is winning
    }
    if(abs(slope)==1){
      p = 0
    }
  }
  return(p)
}

bin_size = 20
bins = seq(300,-300,by=-bin_size)
layer_size = 10
layers = seq(0,150,by=layer_size)

#calculate population of each slice
pd0 <- data %>%
  filter(phase==1,diffCon=="Distance",stage>1,goal_type=="Approach",left_current_distance>0,right_current_distance>0,right_start_distance==30,left_start_distance==150) %>%
  mutate(distance_from_origin = sqrt( (left_start_distance-left_current_distance)^2 + (right_start_distance-right_current_distance)^2  ),
         layer = cut(distance_from_origin,breaks=layers)) %>%
  rowwise() %>%
  mutate(projected_crossing = project_crossing(left_start_distance,right_start_distance,left_current_distance,right_current_distance),
         slice = cut(projected_crossing,breaks=bins)) %>%
  group_by(slice,layer) %>%
  summarise(pop = length(stage))

plot(density(pd0$slice_pop))
plot(pd0$slice,pd0$slice_pop)

#associate bin with a slice
tiles = seq(0,149,by=1)
sds = c(150)
 pd1= expand.grid(left_start_distance=sds,right_start_distance=30,left_current_distance_bin=tiles,right_current_distance_bin=tiles) %>%
  mutate(distance_from_origin = sqrt( (left_start_distance-left_current_distance_bin)^2 + (right_start_distance-right_current_distance_bin)^2  )) %>%
  rowwise() %>%
  mutate(projected_crossing = project_crossing(left_start_distance,right_start_distance,left_current_distance_bin,right_current_distance_bin)) %>%
  group_by(left_current_distance_bin,right_current_distance_bin) %>%
  summarise(slice = cut(projected_crossing,breaks=bins),
            layer= cut(distance_from_origin,breaks=layers))

pd2 <- left_join(pd1,pd0)


ggplot(data=pd2,aes(x=left_current_distance_bin,y=right_current_distance_bin,
                   fill=pop)) +
  geom_tile() +
  #facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_reverse(expand=c(0,0)) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(0,150),xlim=c(0,150)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  #scale_fill_distiller(palette="Spectral", na.value="white",name="Density") +
  scale_fill_gradient2(low="black",high="red",na.value='white') +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))
       # legend.position = 'none')

ggsave("WheelPlot_Approach_30_150.pdf",width=11,height=10)


    left_current_distance_bin = round(left_current_distance/2,-1)*2,
         right_current_distance_bin = round(right_current_distance/2,-1)*2,
         stage_bin = factor(ceiling(stage/10),levels=1:3,labels=c('Stages 1-10','Stages 11-20','Stages 21-30'))  ) %>%
  group_by(left_current_distance_bin,right_current_distance_bin,goal_type) %>%
  summarise(count = length(stage),
            right_prioritised = mean(prioritise_right),
            left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance)) %>%
  filter(left_current_distance>0,right_current_distance>0)




#RAYL PLOT - the number of points that fall in each slice
bin_size = 10
bins = seq(300,-300,by=-bin_size)



project_crossing <- function(x0,y0,x,y){
  #print(c(x,y))
  slope = (y-y0)/(x-x0)
  p = NA
  if(!is.na(slope)){
    if(abs(slope)>1){
      p = (x - y/slope)*-1 #if y is winning
    }
    if(abs(slope)<1){
      p = y - slope*x #if x is winning
    }
    if(abs(slope)==1){
      p = 0
    }
  }
  return(p)
}


x0=150
y0=150
x=150
y=150
p = -7.82612E2
project_crossing(x0,y0,x,y)

#calculate population of each slice
pd0 <- data %>%
  filter(phase==1,diffCon=="Distance",stage>1,goal_type=="Approach",left_current_distance>0,right_current_distance>0,right_start_distance==150,left_start_distance==150) %>%
  rowwise() %>%
  mutate(projected_crossing = project_crossing(left_start_distance,right_start_distance,left_current_distance,right_current_distance),
         slice = cut(projected_crossing,breaks=bins)) %>%
  group_by(slice) %>%
  summarise(slice_pop = length(stage))

plot(density(pd0$slice_pop))
plot(pd0$slice,pd0$slice_pop)

#associate bin with a slice
tiles = seq(0,149,by=1)
sds = c(150)
 pd1= expand.grid(left_start_distance=sds,right_start_distance=150,left_current_distance_bin=tiles,right_current_distance_bin=tiles) %>%
  rowwise() %>%
  mutate(projected_crossing = project_crossing(left_start_distance,right_start_distance,left_current_distance_bin,right_current_distance_bin)) %>%
  group_by(left_current_distance_bin,right_current_distance_bin) %>%
  summarise(mean_projected_crossing = mean(projected_crossing),
            slice = cut(mean_projected_crossing,breaks=bins))

pd2 <- left_join(pd1,pd0)


ggplot(data=pd2,aes(x=left_current_distance_bin,y=right_current_distance_bin,
                   fill=slice_pop)) +
  geom_tile() +
  #facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_reverse(expand=c(0,0)) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(0,150),xlim=c(0,150)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  scale_fill_gradient2(low="black",high="red",na.value='white') +
  theme.goal +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))
       # legend.position = 'none')

ggsave("RadialPlot_Approach_150_150.pdf",width=11,height=10)









pd1 <- data %>%
  filter(phase==1,diffCon=="Distance")


ggplot(data=pd,aes(x=left_current_distance_bin,y=right_current_distance_bin)) +
  geom_tile(aes(fill=right_prioritised)) +
  geom_density2d(data=pd1,colour='black') +
  #geom_raster(data=pd,aes(fill=density))
  ##facet_grid(left_start_distance~right_start_distance) +
  facet_grid(.~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(0,160),xlim=c(0,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0.5) +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"))

ggsave("HeatMapAndDensity.pdf",width=11,height=10)


pd <- data %>%
  filter(phase==1,diffCon=="Distance") %>%
  mutate(left_current_distance_bin = round(left_current_distance*0.5,-1)/0.5,
         right_current_distance_bin = round(right_current_distance*0.5,-1)/0.5,
         stage_bin = factor(ceiling(stage/10),levels=1:3,labels=c('Stages 1-10','Stages 11-20','Stages 21-30'))  ) %>%
  group_by(left_current_distance_bin,right_current_distance_bin,goal_type) %>%
  summarise(count = length(stage),
            right_prioritised = mean(prioritise_right),
            left_current_distance = mean(left_current_distance),
            right_current_distance = mean(right_current_distance),
            movement_right = mean(right_start_distance-min(right_current_distance)),
            movement_left = mean(left_start_distance-min(left_current_distance)))
scalar=.1

ggplot(data=pd,aes(x=left_current_distance_bin,y=right_current_distance_bin)) +
  geom_raster(aes(fill=right_prioritised),interpolate = F) +
  #facet_grid(left_start_distance~right_start_distance) +
  facet_grid(.~goal_type) +
  #geom_contour(aes(x=left_current_distance_bin,y=right_current_distance_bin,z=right_prioritised),colour="black",binwidth=.1,size=.2) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(0,160),xlim=c(0,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0.5) +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times")) +
  geom_segment(aes(x=left_current_distance_bin+movement_left*scalar,
               y=right_current_distance_bin+movement_right*scalar,
               xend=left_current_distance_bin-movement_left*scalar,
               yend=right_current_distance_bin-movement_right*scalar),
               arrow=arrow(length=unit(0.05,"inches")))
#legend.position = 'none')

ggsave("HeatMapAndVectorFields.pdf",width=11,height=10)



















library(plotly)
# volcano is a numeric matrix that ships with R
p <- plot_ly(z = ~volcano) %>% add_surface()

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="surface/1")
chart_link

SN = c(1:100, 1:100, 1:100, 1:100)
class = c(rep("A1", 100), rep("B2", 100), rep("C3", 100), rep("D4", 100)) # total 6000 levels
myvar = rnorm(400)
mydf = data.frame(SN, class, myvar)
dcast(mydf, SN ~ class)

library(plot3D)
library(reshape2)
library(dplyr)

pd <- data %>%
  filter(phase==1,diffCon=="Distance",goal_type=='Approach') %>%
  mutate(left_current_distance_bin = round(left_current_distance*1,-1)/1,
         right_current_distance_bin = round(right_current_distance*1,-1)/1) %>%
         group_by(left_current_distance_bin,right_current_distance_bin) %>%
         summarise(count = length(stage),
                   prioritise_right = mean(prioritise_right)) %>%
  filter(right_current_distance_bin>-20, left_current_distance_bin>-20,
         right_current_distance_bin<=165, left_current_distance_bin<=165)


count = as.matrix(dcast(pd,left_current_distance_bin~right_current_distance_bin, value.var="count"))[,2:(length(unique(pd$left_current_distance_bin))+1)]
prioritise_right = as.matrix(dcast(pd,left_current_distance_bin~right_current_distance_bin, value.var="prioritise_right"))[,2:(length(unique(pd$left_current_distance_bin))+1)]

#library(grDevices)

brColors = colorRampPalette(c("blue","white","red"))( 9 )

postscript("3D-Approach.eps", width=10, height=10, paper="special", family="Times",horizontal=FALSE, onefile=FALSE)


persp3D(x=unique(pd$left_current_distance_bin),y=unique(pd$right_current_distance_bin),
        z = count,colvar=prioritise_right,col=brColors, zlim = c(0, 5000), phi = 45,theta=120,
        colkey = list(length = 0.4, width = 0.8, shift = 0.15,
                      cex.axis = 0.8, cex.clab = 0.85), lighting = TRUE, lphi = 80,
        clab = c("","height","m"), bty = "f", plot = TRUE,axes=T,
        xlab="Left Distance to Goal",
        ylab="Right Distance to Goal",
        zlab="Frequency of Score Combination",
        ticktype="detailed")

dev.off()









snapshot3d( file.path("3D-Aproach.png"), top = TRUE )
# create gradient in x-direction
Vx <- volcano[-1, ] - volcano[-nrow(volcano), ]-9900
# add as image with own color key, at bottom
image3D(z = -60, colvar = Vx/10, add = TRUE,
        colkey = list(length = 0.2, width = 0.4, shift = -0.15,
                      cex.axis = 0.8, cex.clab = 0.85),
        clab = c("","gradient","m/m"), plot = TRUE)
# add contour
contour3D(z = -60+0.01, colvar = Vx/10, add = TRUE,
          col = "black", plot = TRUE)









#legend.position = 'none')
x=seq(10,15,by=0.25)
y=seq(40,50,by=0.25)
u=matrix(runif(length(x)*length(y),-2,3),nrow=length(x),ncol=length(y))
v=matrix(runif(length(x)*length(y),-2,3),nrow=length(x),ncol=length(y))
#note that I corrected these

#melt for plotting
library(reshape2)
u <- melt(u,value.name = "u")
v <- melt(v,value.name = "v")
wind <- merge(u, v)
wind$x <- x[wind[,1]]
wind$y <- y[wind[,2]]


#plot
library(ggplot2)
library(grid)
scaler <- 1

p <- ggplot(wind, aes(x=x, y=y, xend=x+u*scaler, yend=y+v*scaler)) + geom_segment(arrow=arrow())
print(p)
#interpoloation

#arrows

#3d

library("MASS")
data(geyser, "MASS")
m <- ggplot(geyser, aes(x = duration, y = waiting)) +
  geom_point() + xlim(0.5, 6) + ylim(40, 110)
m + geom_density2d()


#dividing up by easy vs hard
pd_tmp <- data %>%
  filter(phase==1,left_current_distance>0,right_current_distance>0) %>%
  mutate(prioritise_farther = (left_current_distance>right_current_distance)*(1-prioritise_right) +
  (left_current_distance<right_current_distance)*(prioritise_right))

pd_tmp$prioritise_farther[which(pd_tmp$left_current_distance==pd_tmp$right_current_distance)] = NA

pd_tmp <- pd_tmp %>%
  group_by(subject,trial_number) %>%
  summarise(prop_farther = mean(prioritise_farther,na.rm=T))


  #  prop_farther =  sum(prioritise_farther,is.na=T)  / sum(!is.na(prioritise_farther) ) )



     plot(density(pd_tmp$prop_farther))



pd <- left_join(data,pd_tmp,by=c('subject','trial_number')) %>%
  filter(diffCon=='Distance',goal_type=='Approach') %>%
  group_by(left_start_distance,right_start_distance,stage,subject)

ggplot(data=subset(pd,prop_farther>0.5),aes(x=left_current_distance,y=right_current_distance,group=subject,colour=subject)) +
  geom_path() +
  facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')


ggsave("IndividualTrajectoriesBalanced.pdf",width=11,height=10)

ggplot(data=subset(pd,prop_farther<0.5),aes(x=left_current_distance,y=right_current_distance,group=subject,colour=subject)) +
  geom_path() +
  facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')


ggsave("IndividualTrajectoriesSequential.pdf",width=11,height=10)









ggplot(data=subset(pd,goal_type=='Avoidance'),
       aes(x=left_current_distance,y=right_current_distance,group=movement_group,colour=factor(movement_group))) +
  geom_path() +
  facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')

ggsave("TrajectoryDistributionAvoidance.pdf",width=11,height=10)






#dividing up by quantile - folded

find_closer <- function(x1,x2){
  #print(typeof(x1))
  #print(x1[1])
  if(x1[1]<x2[1]){
    y=rep(1,length(x1))
  }
  if(x1[1]>x2[1]){
    y=rep(2,length(x1))
  }
  if(x1[1]==x2[1]){
    if(x1[2]<x2[2]){
      y=rep(1,length(x1))
    }
    if(x1[2]>x2[2]){
      y=rep(2,length(x1))
    }
  }
  return(y)
}

pd_tmp <- data %>%
  filter(phase==1,left_current_distance>0,right_current_distance>0,diffCon=='Distance') %>%
  group_by(subject,trial_number,left_start_distance,right_start_distance,goal_type) %>%
  mutate(closer_goal = find_closer(left_current_distance,right_current_distance),
         closer_start_distance = (closer_goal==1)*left_start_distance + (closer_goal==2)*right_start_distance ,
         closer_current_distance = (closer_goal==1)*left_current_distance + (closer_goal==2)*right_current_distance ,
         farther_start_distance = (closer_goal==2)*left_start_distance + (closer_goal==1)*right_start_distance ,
         farther_current_distance = (closer_goal==2)*left_current_distance + (closer_goal==1)*right_current_distance) %>%
  summarise(closer_goal = mean(closer_goal),
         movement_closer = mean(closer_start_distance - min(closer_current_distance)),
         movement_farther = mean(farther_start_distance - min(farther_current_distance)),
         movement_diff = movement_closer - movement_farther,
         movement_group = NaN,
         closer_start_distance = mean(closer_start_distance),
         farther_start_distance = mean(farther_start_distance))

gs = c('Approach','Avoidance')
ds = c(30,45,67.5,90,112.5,135,150)

for(g in gs){
  for(c_ds in ds){
    for(f_ds in ds){

      if(c_ds<=f_ds){

        movement_diff = pd_tmp$movement_diff[which((pd_tmp$closer_start_distance==c_ds)&
                                                   (pd_tmp$farther_start_distance==f_ds)&
                                                   (pd_tmp$goal_type==g))]
        breaks = quantile(movement_diff,probs=seq(0,1,by=0.25))
        breaks[1] = -Inf
        breaks[length(breaks)] = Inf

        pd_tmp[which((pd_tmp$closer_start_distance==c_ds)&
                     (pd_tmp$farther_start_distance==f_ds)&
                     (pd_tmp$goal_type==g)),'movement_group'] <- factor(cut(movement_diff,breaks=breaks),labels=1:(length(breaks)-1))

      }
    }
  }
}

pd_tmp1 = data %>% filter(diffCon=='Distance') %>% select(-goal_type,-closer_start_distance,-farther_start_distance,-closer_current_distance,-farther_current_distance)
pd_tmp2 = pd_tmp %>% ungroup()


pd <- left_join(pd_tmp1,pd_tmp2,by=c('subject','trial_number')) %>%
  mutate(closer_current_distance = (closer_goal==1)*left_current_distance + (closer_goal==2)*right_current_distance,
         farther_current_distance = (closer_goal==2)*left_current_distance + (closer_goal==1)*right_current_distance) %>%
  group_by(farther_start_distance,closer_start_distance,goal_type,movement_group,stage) %>%
  summarise(farther_current_distance = mean(farther_current_distance),
            closer_current_distance = mean(closer_current_distance)) %>%
  mutate(movement_groupF = factor(movement_group ))



ggplot(data=subset(pd,goal_type=='Approach'),
       aes(x=farther_current_distance,y=closer_current_distance,group=movement_group,colour=factor(movement_group))) +
  geom_path() +
  facet_grid(closer_start_distance~farther_start_distance) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')

ggsave("TrajectoryDistributionApproach.pdf",width=11,height=10)




ggplot(data=subset(pd,goal_type=='Avoidance'),
       aes(x=left_current_distance,y=right_current_distance,group=movement_group,colour=factor(movement_group))) +
  geom_path() +
  facet_grid(left_start_distance~right_start_distance) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  coord_cartesian(ylim= c(-100,160),xlim=c(-100,160)) +
  xlab('Left Distance to Goal') +
  ylab("Right Distance to Goal") +
  theme.goal +
  #scale_fill_distiller(palette="Spectral", na.value="white",name=" ") +
  theme(panel.margin = unit(1, "lines"),
        strip.text.x = element_text(size=8,face="bold",colour="black",family="Times"),
        strip.text.y = element_text(size=8, face="bold","black",family="Times"),
        axis.text.x = element_text(size=6,colour="black",family="Times"),
        axis.text.y = element_text(size=6,colour="black",family="Times"),
        legend.position = 'none')

ggsave("TrajectoryDistributionAvoidance.pdf",width=11,height=10)



 #Distributions of goal achievement (not very clear)
 pd = data %>%
  filter(phase==1,diffCon=="Distance",goal_type=="Avoidance") %>%
  group_by(subject,trial_number) %>%
  summarise(left_goal_first_reached = 1*which(left_current_distance<=0)[1],
         right_goal_first_reached = 1*which(right_current_distance<=0)[1],
         left_start_distance = mean(left_start_distance),
         right_start_distance = mean(right_start_distance),
         left_start_deadline = mean(left_start_deadline),
         right_start_deadline = mean(right_start_deadline))


# pd=left_join(data,data_tmp,by=c('subject','trial_number'))

 ggplot(data=pd) +
   geom_density(aes(x=left_goal_first_reached),fill='blue',alpha=0.5) +
   geom_density(aes(x=right_goal_first_reached),fill='red',alpha=0.5) +
   facet_grid(left_start_deadline ~ right_start_deadline)




#Choice over time
 pd = data %>%
  filter(phase==1,diffCon=="Distance") %>%
  mutate(closer_start_distanceF = factor(left_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Closest','Closer','Close','Moderate','Far','Farther','Farthest')),
         further_start_distanceF = factor(right_start_distance,levels=c(30,45,67.5,90,112.5,135,150),labels=c('Closest','Closer','Close','Moderate','Far','Farther','Farthest'))   ) %>%
 group_by(closer_start_distanceF,further_start_distanceF,stage,goal_type) %>%
   summarise(obs_choice = mean(prioritise_right*(right<     )        ),
             opt_choice = mean(policy_right))


 ggplot(data=pd) +
   geom_line(aes(x=stage,y=obs_choice,group=goal_type,colour=goal_type)) +
      facet_grid(further_start_distanceF ~ closer_start_distanceF) +
   coord_cartesian(ylim= c(0,1)) + theme.goal







 #%>%



 ggplot(data=data,aes(x=response_time)) +
  geom_histogram() +
  facet_grid(correct ~ coherence) +
  coord_cartesian(xlim=c(0,1500))


   facet_wrap(diffCon~goal_type)








as.data.frame(ad)










