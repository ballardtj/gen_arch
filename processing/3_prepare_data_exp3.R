#clear workspace
rm(list=ls())

#load packages
library(tidyverse)
library(rstan)


#Load Data

#NOTE: Currently have not implemented dynamic programming for expts 3 and 4!

#load("./data/clean/dp_data.RData") #v4 is most current with bin size = 2

load("data/clean/transformed_data.RData")

data_bound_tmp = transformed_data %>%
  filter(trial_kind == "experimental") %>%
  mutate(left_start_deadline = left_days_total,
         right_start_deadline = right_days_total,
  phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2) %>%
  filter(right_current_distance>0,left_current_distance>0,phase==1) %>%
  mutate(max_dl = case_when(
            expt < 3 ~ 91,
            expt >= 3 ~ 9
          ),
         a_d = ceiling(right_current_distance) /201, #formerly 188
         b_d = ceiling(left_current_distance) /201,  #formerly 188
         a_t = ceiling(right_days_remaining) / max_dl,
         b_t = ceiling(left_days_remaining) / max_dl,
         a_d0 = right_start_distance,
         b_d0 = left_start_distance,
         a_t0 = right_start_deadline,
         b_t0 = left_start_deadline) #%>%
  #filter(stage<4)

data_bound_tmp$s = as.numeric(as.factor(data_bound_tmp$subject)) #for some reason this does not work in dplyr

goal_types = c('ap','av')
sources = c('obs','opt')

for(source in 1){
  for(frame in 1:2){

    #Remove observations where one or more goals is above threshold

    if(frame==1){
      Bdata <- data_bound_tmp %>% filter(goal_type=="Approach")
    }

    if(frame==2){
      Bdata <- data_bound_tmp %>% filter(goal_type=="Avoidance")
    }

    #if optimal data, remove observations where policy = 0.5 (equal left vs right)
    #if(source==2){
    #  Bdata = Bdata %>% filter(policy!=0.5)
    #}

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
      a_logd[subj,1:n] = log(Bdata_tmp$a_d)
      b_logd[subj,1:n] = log(Bdata_tmp$b_d)
      a_logt[subj,1:n] = log(Bdata_tmp$a_t)
      b_logt[subj,1:n] = log(Bdata_tmp$b_t)
      a_dot[subj,1:n] = Bdata_tmp$a_d / Bdata_tmp$a_t
      b_dot[subj,1:n] = Bdata_tmp$b_d / Bdata_tmp$b_t
      a_tod[subj,1:n] = Bdata_tmp$a_t / Bdata_tmp$a_d
      b_tod[subj,1:n] = Bdata_tmp$b_t / Bdata_tmp$b_d
      if(sources[source] == "obs"){
        y[subj,1:n] = Bdata_tmp$prioritise_right
      }
      if(sources[source] == "opt"){
        y[subj,1:n] = 1*(runif(length(Bdata_tmp$ev_left)) < (Bdata_tmp$ev_right / (Bdata_tmp$ev_right+Bdata_tmp$ev_left)))     #Bdata_tmp$policy
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
      Maxobs = Maxobs,
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

    # dataList = list(
    #   Ntotal = length(Bdata$s) ,
    #   s = Bdata$s,
    #   Nsubj = Nsubj,
    #   a_logt = log(Bdata$a_t),
    #   b_logt = log(Bdata$b_t),
    #   y = Bdata$prioritise_right
    # )

    #dataList$y_opt = Bdata$policy_right
    #save(dataList,file=paste0("./data/clean/",sources[source],"_",goal_types[frame],'_datalist_tp.Rda'))

    #Save as rdump
    list2env(dataList, .GlobalEnv)
    stan_rdump(names(dataList),file=paste0('./data/clean/',sources[source],"_",goal_types[frame],'_rdump_expt3.R'))
  }

}






