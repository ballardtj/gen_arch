### clear workspace ###

rm(ls=list())

### load packages ###

library(rstan)
library(coda)
library(knitr)
library(tidybayes)
library(tidyverse)

### Experiments 1, 2, and 3 ###

parms = c('w1_mean','w1_sd','w2_mean','w2_sd','w3_mean','w3_sd',
  'delta_mean','delta_sd','tau_mean','tau_sd','alpha_mean','alpha_sd')

# Approach - Obs

load("data/derived/expt123_obs_ap_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))




# Avoidance

load("data/derived/expt123_obs_av_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))

# Approach

load("data/derived/expt123_opt_ap_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))

# Avoidance

load("data/derived/expt123_opt_av_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))



### Get and save unnormalised posteriors on parameters ###


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
  for (source in c('obs','opt')){
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

#unnormalised posteriors do not have weights that sum to one.
save(posts,file="data/derived/unnormalised_posteriors.RData")

#normalise posteriors so that weights sum to one.
posts_norm = posts %>%
  spread(key=parameter,value=value) %>%
  mutate(w1_0 = replace_na(w1,0),
         w2_0 = replace_na(w2,0)) %>%
  mutate(s = w1_0 + w2_0 + w3,
         w1 = w1/s,
         w2 = w2/s,
         w3 = w3/s) %>%
    select(subject,.draw,goal_type,source,alpha,delta,tau,w1,w2,w3,s) %>%
    gather(key=parameter,value=value,alpha:s)


# Mean across participants of parameters from models of observed and optimal decisions
pd1 = posts_norm %>%
  group_by(.draw,parameter,goal_type,source) %>%
  summarise(estimate = mean(value,na.rm=T))

#Mean parameters across conditions
ggplot(data=pd1) +
  geom_density(aes(x=estimate,fill=source),alpha=0.5) +
  facet_grid(goal_type~parameter,scale="free")

# 95% CI for each participant of parameters of model of observed decisions

pd3=posts_norm %>%
  filter(source=="obs") %>%
  group_by(parameter,subject,goal_type) %>%
  point_interval(value) %>%
  group_by(parameter,goal_type) %>%
  arrange(value) %>%
  mutate(order = 1:n())

ggplot(data=pd3) +
  geom_pointintervalh(aes(y=order,x=value,show.legend=T,size=0.1,alpha=0.2)) +
  facet_grid(goal_type~parameter,scale="free") + theme(legend.position="bottom")




  #correlation between the parameters in different conditions and experiments
  library(GGally)

  posts_norm %>%
    spread(key = parameter,value=value) %>%
    mutate(expt = case_when(
      is.na(w2) ~ 1,
      is.na(w1) ~ 2,
      !is.na(w1) & !is.na(w2) ~ 3)) %>%
    gather(key = parameter,value = value,alpha:w3) %>%
    group_by(subject,source,goal_type,parameter) %>%
    mutate(mean_value = mean(value),
           value_mc = value - mean_value) %>%
    filter(goal_type=="ap",source=="obs",expt==3) %>%
    select(expt,subject,.draw,parameter,value_mc) %>%
    spread(key = parameter,value=value_mc) %>%
    ggpairs(.,columns=6:12)


  #mean across experiment
  across_expt = posts_norm %>%
    filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
    spread(key = parameter,value=value) %>%
    mutate(expt = case_when(
      is.na(w2) ~ 1,
      is.na(w1) ~ 2,
      !is.na(w1) & !is.na(w2) ~ 3)) %>%
    gather(key = parameter,value = value,w1:w3) %>%
    spread(key = source, value = value) %>%
    mutate(bias = obs - opt) %>%
    group_by(subject,goal_type,parameter) %>%
    summarise(subject_mean = mean(bias,na.rm=T)) %>%
    group_by(goal_type,parameter) %>%
    summarise(upper = quantile(subject_mean,.975,na.rm=T),
              lower = quantile(subject_mean,.025,na.rm=T),
              mean = mean(subject_mean,na.rm=T))

  print(across_expt)


  #mean within each experiment
  within_expt = posts_norm %>%
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
    summarise(subject_mean = mean(bias,na.rm=T)) %>%
    group_by(goal_type,parameter,expt) %>%
    summarise(upper = quantile(subject_mean,.975,na.rm=T),
              lower = quantile(subject_mean,.025,na.rm=T),
              mean = mean(subject_mean,na.rm=T))

  print(within_expt)



