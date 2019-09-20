rm(list=ls())


tibble(t = seq(0.01,.99,.01),
       w2 = 0.2,
       tau = 0.5,
       tg = w2*t^tau) %>%
  ggplot(aes(x=t,y=tg)) +
  geom_line() +
  coord_cartesian(ylim=c(0,1))




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


#Get minimum and maximum of each person
# IMPORT DATA -------

load("data/clean/dp_data_expt12.RData")
data_bound_expt12 = data_bound

load("data/clean/dp_data_expt3.RData")
data_bound_expt3 = data_bound

data_bound = rbind(data_bound_expt12,data_bound_expt3) %>%
  mutate(phase = (day<=pmin(right_start_deadline,left_start_deadline)) +  (day>pmin(right_start_deadline,left_start_deadline))*2)

# # PREPARE DATA FOR VECTOR PLOT -------

subject_ranges = data_bound %>% ungroup() %>%
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
  group_by(subject) %>%
  summarise(d_min = min(c(a_d,b_d)),
            d_max = max(c(a_d,b_d)),
            t_min = min(c(a_t,b_t)),
            t_max = max(c(a_t,b_t)),
            dot_min = min(c(a_d / a_t,b_d/b_t)),
            dot_max = max(a_d / a_t))

pd = left_join(posts_norm,subject_ranges) %>%
  spread(key = parameter,value=value) %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3)) %>%
  mutate(sg_at_d_min = (delta>=0)*w1*d_min^delta + (delta<0)*w1*(1-d_min^-delta),
         sg_at_d_max = (delta>=0)*w1*d_max^delta + (delta<0)*w1*(1-d_max^-delta),
         tg_at_t_min = (tau>=0)*w2*t_min^tau + (tau<0)*w2*(1-t_min^-tau),
         tg_at_t_max = (tau>=0)*w2*t_max^tau + (tau<0)*w2*(1-t_max^-tau),
         stg_max = w3,#*2*(1-alpha)*sqrt(alpha/(1-alpha)),
         #stg_at_dot_min = stg_max / (alpha/dot_min + (1-alpha)*dot_min),
         #stg_at_dot_max = stg_max / (alpha/dot_max + (1-alpha)*dot_max),
         sg_max = pmax(sg_at_d_min,sg_at_d_max),
         tg_max = pmax(tg_at_t_min,tg_at_t_max)) %>%
  select(subject,.draw,source,goal_type,expt,sg_max,tg_max,stg_max) %>%
  gather(key = parameter,value = value,sg_max:stg_max) %>%
  spread(key = source, value = value) %>%
  mutate(bias = obs - opt) %>%
  group_by(subject,goal_type,parameter,expt) %>%
  summarise(upper = quantile(bias,.975,na.rm=T),
            lower = quantile(bias,.025,na.rm=T),
            mean = mean(bias,na.rm=T))


# pd = posts_norm %>%
#   #filter(parameter == "w1" | parameter == "w2" | parameter == "w3") %>%
#   group_by(parameter,goal_type,source) %>%
#   summarise(value = mean(value,na.rm=T)) %>% data.frame()
#
# pd = posts_norm %>%
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
#   summarise(upper = quantile(bias,.975,na.rm=T),
#             lower = quantile(bias,.025,na.rm=T),
#             mean = mean(bias,na.rm=T))

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
  mutate(parameter = factor(parameter,levels=c("sg_max","tg_max","stg_max"),labels=c('Spatial','Temporal','Spatiotemporal')),
         goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))

expt_data = left_join(posts_norm,subject_ranges) %>%
  spread(key = parameter,value=value) %>%
  mutate(expt = case_when(
    is.na(w2) ~ 1,
    is.na(w1) ~ 2,
    !is.na(w1) & !is.na(w2) ~ 3)) %>%
  mutate(sg_at_d_min = (delta>=0)*w1*d_min^delta + (delta<0)*w1*(1-d_min^-delta),
         sg_at_d_max = (delta>=0)*w1*d_max^delta + (delta<0)*w1*(1-d_max^-delta),
         tg_at_t_min = (tau>=0)*w2*t_min^tau + (tau<0)*w2*(1-t_min^-tau),
         tg_at_t_max = (tau>=0)*w2*t_max^tau + (tau<0)*w2*(1-t_max^-tau),
         stg_max = w3,#*2*(1-alpha)*sqrt(alpha/(1-alpha)),
         #stg_at_dot_min = stg_max / (alpha/dot_min + (1-alpha)*dot_min),
         #stg_at_dot_max = stg_max / (alpha/dot_max + (1-alpha)*dot_max),
         sg_max = pmax(sg_at_d_min,sg_at_d_max),
         tg_max = pmax(tg_at_t_min,tg_at_t_max)) %>%
  select(subject,.draw,source,goal_type,expt,sg_max,tg_max,stg_max) %>%
  gather(key = parameter,value = value,sg_max:stg_max) %>%
  spread(key = source, value = value) %>%
  mutate(bias = obs - opt) %>%
  group_by(goal_type,parameter,expt,.draw) %>%
  summarise(mean_bias = mean(bias,na.rm=T)) %>%
  ungroup() %>%
  mutate(parameter = factor(parameter,levels=c("sg_max","tg_max","stg_max"),labels=c('Spatial','Temporal','Spatiotemporal')),
         goal_type = factor(goal_type,levels=c('ap','av'),labels=c('Approach','Avoidance')))




  posts_norm %>%
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


