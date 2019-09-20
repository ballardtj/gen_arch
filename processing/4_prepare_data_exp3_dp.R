#clear workspace
rm(list=ls())

#load packages
library(tidyverse)
library(rstan)


#Load Data

#NOTE: Currently have not implemented dynamic programming for expts 3 and 4!

#load("./data/clean/dp_data.RData") #v4 is most current with bin size = 2

load("data/clean/dp_data_expt12.RData")
data_bound_expt12 = data_bound

load("data/clean/dp_data_expt3.RData")
data_bound_expt3 = data_bound

#load("data/clean/transformed_data.RData")

data_bound_tmp = rbind(data_bound_expt12,data_bound_expt3) %>%
  mutate(left_start_deadline = left_days_total,
         right_start_deadline = right_days_total,
  phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(right_current_distance>0,left_current_distance>0,phase==1,expt<4) %>%
  mutate(max_dl = case_when(
            expt < 3 ~ 91,
            expt >= 3 ~ 9
          ),
         a_d = ceiling(right_current_distance) /188, #needs to be 201 if modeling expt 4
         b_d = ceiling(left_current_distance) /188,  #as above
         a_t = ceiling(right_days_remaining) / max_dl,
         b_t = ceiling(left_days_remaining) / max_dl,
         a_d0 = right_start_distance,
         b_d0 = left_start_distance,
         a_t0 = right_start_deadline,
         b_t0 = left_start_deadline) #%>%
  #filter(stage<4)


# data_bound_tmp %>%
#   filter(expt==3,goal_type=="Approach") %>%
#   summarise(max(a_d),max(b_d)) %>% as.data.frame()
#
#   #Get proportion for each trial
#   group_by(subject,a_d0,b_d0,a_t0,b_t0) %>%
#   summarise(y_obs_tr = mean(prioritise_right)) %>%
#   #Get proportion for each condition
#   group_by(a_d0,b_d0,a_t0,b_t0) %>%
#   summarise(y_obs_con = mean(y_obs_tr),
#             y_obs_con_se = sd(y_obs_tr)/sqrt(length(y_obs_tr)),
#             y_obs_hi = y_obs_con + mean(y_obs_con_se),
#             y_obs_lo = y_obs_con - mean(y_obs_con_se)) %>%
# filter(b_t0==8) %>%
# ggplot(aes(x=factor(a_t0),group=factor(b_t0))) +
#   #geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#   #geom_line(aes(y=y_pred_mean),col="blue") +
#   geom_line(aes(y=y_obs_con),col="red") +
#   geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
#   facet_grid(factor(b_d0,labels=paste("Left Distance:",levels(factor(b_d0))))
#              ~factor(a_d0,labels=paste("Right Distance:",levels(factor(a_d0))))) +
#   coord_cartesian(ylim=c(0,1)) +
#   labs(x="Right Starting Distance",y=" ") +
#   theme(legend.position="none")



data_bound_tmp$s = as.numeric(as.factor(data_bound_tmp$subject)) #for some reason this does not work in dplyr

goal_types = c('ap','av')
sources = c('obs','opt')

for(source in 1:2){
  for(frame in 1:2){

    #Remove observations where one or more goals is above threshold

    if(frame==1){
      Bdata <- data_bound_tmp %>% filter(goal_type=="Approach")
    }

    if(frame==2){
      Bdata <- data_bound_tmp %>% filter(goal_type=="Avoidance")
    }

    #if optimal data, remove observations where policy = 0.5 (equal left vs right)
    if(source==2){
     Bdata = Bdata %>% filter(policy!=0.5)
    }

    # #Create vector of length Nsubj that represents the experiment
    Exptdata = Bdata %>% group_by(s) %>% summarise(expt = mean(expt))
    Exptdata$s_sg=NA
    Exptdata$s_tg=NA

    ctr_sg=0
    ctr_tg=0

    for(s in 1:dim(Exptdata)[1]){
      if(Exptdata$expt[s]==1){
        ctr_sg=ctr_sg+1
        Exptdata$s_sg[s]=ctr_sg
        Exptdata$s_tg[s]=0
      }
      if(Exptdata$expt[s]==2){
        ctr_tg=ctr_tg+1
        Exptdata$s_sg[s]=0
        Exptdata$s_tg[s]=ctr_tg
      }
      if(Exptdata$expt[s]==3){
        ctr_sg=ctr_sg+1
        ctr_tg=ctr_tg+1
        Exptdata$s_sg[s]=ctr_sg
        Exptdata$s_tg[s]=ctr_tg
      }
    }

    #make a_logd to b_tod arrays
    counts = Bdata %>% group_by(s) %>% summarise(count = length(a_d))
    Maxobs = max(counts$count)
    Nsubj = max(counts$s)
    Nobs = rep(NA,Nsubj)

    a_logd = matrix(1,Nsubj,Maxobs)
    b_logd = matrix(1,Nsubj,Maxobs)
    a_logt = matrix(1,Nsubj,Maxobs)
    b_logt = matrix(1,Nsubj,Maxobs)
    a_dot = matrix(1,Nsubj,Maxobs)
    b_dot = matrix(1,Nsubj,Maxobs)
    a_tod = matrix(1,Nsubj,Maxobs)
    b_tod = matrix(1,Nsubj,Maxobs)
    y = matrix(1,Nsubj,Maxobs)
    a_d0 = matrix(1,Nsubj,Maxobs)
    b_d0 = matrix(1,Nsubj,Maxobs)
    a_t0 = matrix(1,Nsubj,Maxobs)
    b_t0 = matrix(1,Nsubj,Maxobs)

    for(subj in 1:length(unique(Bdata$s))){
      Bdata_tmp = Bdata %>% filter(s==subj)
      n = dim(Bdata_tmp)[1]
      Nobs[subj]=n

      if(any(Bdata_tmp$a_d>1)){
        break()
      }

      #Rounded the digits to the second decimal place to make computation a bit easier
      a_logd[subj,1:n] = pmin( round( log(Bdata_tmp$a_d), 2) , -0.01)
      b_logd[subj,1:n] = pmin( round(log(Bdata_tmp$b_d), 2) , -0.01)
      a_logt[subj,1:n] = pmin( round(log(Bdata_tmp$a_t), 2) , -0.01)
      b_logt[subj,1:n] = pmin( round(log(Bdata_tmp$b_t), 2) , -0.01)
      a_dot[subj,1:n] = pmax( round( Bdata_tmp$a_d / Bdata_tmp$a_t, 2 ), 0.01 )
      b_dot[subj,1:n] = pmax( round( Bdata_tmp$b_d / Bdata_tmp$b_t, 2 ), 0.01 )
      a_tod[subj,1:n] = pmax( round( Bdata_tmp$a_t / Bdata_tmp$a_d, 2 ), 0.01 )
      b_tod[subj,1:n] = pmax( round( Bdata_tmp$b_t / Bdata_tmp$b_d, 2 ), 0.01 )
      if(sources[source] == "obs"){
        y[subj,1:n] = Bdata_tmp$prioritise_right
      }
      if(sources[source] == "opt"){
        y[subj,1:n] = Bdata_tmp$policy
      }
      a_d0[subj,1:n] = Bdata_tmp$a_d0
      b_d0[subj,1:n] = Bdata_tmp$b_d0
      a_t0[subj,1:n] = Bdata_tmp$a_t0
      b_t0[subj,1:n] = Bdata_tmp$b_t0
    }

    # Specify the data in a list, for later shipment to Stan:
    dataList = list(
      Ntotal = length(Bdata$s) ,
      s = Bdata$s,
      Nsubj = Nsubj,
      Nobs = Nobs,
      Max_obs = Maxobs,
      a_logd = t(a_logd),
      b_logd = t(b_logd),
      a_logt = t(a_logt),
      b_logt = t(b_logt),
      a_dot = t(a_dot),
      b_dot = t(b_dot),
      a_tod = t(a_tod),
      b_tod = t(b_tod),
      expt = Exptdata$expt,
      s_sg = Exptdata$s_sg,
      s_tg = Exptdata$s_tg,
      Nsubj_sg = max(Exptdata$s_sg),
      Nsubj_tg = max(Exptdata$s_tg),
      y = t(y),
      a_d0 = t(a_d0),
      b_d0 = t(b_d0),
      a_t0 = t(a_t0),
      b_t0 = t(b_t0)
    )

    #Create data for map_rect function
    dataList$real_data = cbind(a_logd, b_logd, a_logt, b_logt, a_dot , b_dot , a_tod, b_tod)
    dataList$int_data = cbind(Nobs,Maxobs,y)
    colnames(dataList$int_data) = NULL

    #Save as rdump
    list2env(dataList, .GlobalEnv)
    stan_rdump(names(dataList),file=paste0('./data/clean/',sources[source],"_",goal_types[frame],'_rdump_expt123.R'))

  }

}

