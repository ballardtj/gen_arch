

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)

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
    mutate(subject = which(dataList$s1>0)[number]) %>%
    select(subject,.draw,parameter,.value) %>%
    spread(key=parameter,value=.value) %>%
    mutate(delta = (w1>=0)*delta + (w1<0)*(-delta),
           w1 = abs(w1)) %>%
    gather(key=parameter,value=value,delta:w1)

  posts_tg = filter(posts_tmp2,parameter %in% c('w2','tau'))  %>%
    mutate(subject = which(dataList$s2>0)[number]) %>%
    select(subject,.draw,parameter,.value) %>%
    spread(key=parameter,value=.value) %>%
    mutate(tau = (w2>=0)*tau + (w2<0)*(-tau),
           w2 = abs(w2)) %>%
    gather(key=parameter,value=value,tau:w2)

  posts_stg = filter(posts_tmp2,parameter %in% c('w3','alpha')) %>%
    mutate(subject = number,
           value=.value) %>%
    select(subject,.draw,parameter,value)

  posts = bind_rows(posts_sg,posts_tg,posts_stg) #%>%
  # ungroup() %>%
  #select(subject,.draw,parameter,.value) %>%
  #spread(key=parameter,value=.value) %>%
  # mutate(delta = if_else( w1 >= 0, delta, -delta),
  #        tau = -tau,
  #        w1 = abs(w1),
  #        w2 = abs(w2)) %>%
  #gather(key=parameter,value=estimate,3:ncol(.)) %>%  #contains(parm_list[[model]])) %>%
  #mutate(parameter = factor(parameter,levels=parm_list[[model]])) %>%
  #filter(!is.na(estimate))
  return(posts)
}

parm_list=list(
  space = c('w1','w2','w3',
            'delta','tau','alpha'),
  nospace = c('w1','w2','delta','tau')
)

source_labels=c(obs="Observed Decisions",opt="Optimal Decisions")
frame_labels=c(ap="Approach Condition",av="Avoidance Condition")
structure_labels=c(hier="Hierarchical Model",fixed="Non-hierarchical Model")
model_labels=c(space="Spatiotemporal Gradient Included",nospace='Spatiotemporal Gradient Omitted')

post_list=list()
ctr=0
for (source in c('obs')){
  for (frame in c('ap','av')){
    for (structure in c('hier')){
      for (model in c('space')){

        dataList=read_rdump(paste0('data/clean/',source,'_',frame,'_rdump.R'))

        load(paste0("data/derived/",frame,"_",source,"_",structure,"_",model,"_fit.RData"))

        ctr=ctr+1
        posts = get_posts(fit,model,dataList,samples)
        posts$frame = frame
        posts$source = source
        posts$structure = structure
        posts$model = model
        post_list[[ctr]] = posts
      }
    }
  }
}

posts = bind_rows(post_list)
rm(post_list)

# Mean across participants of parameters from model of observed decisions
pd1 = posts %>%
  filter(source=="obs") %>%
  group_by(.draw,parameter,frame,structure,model) %>%
  summarise(estimate = mean(value))

#Mean parameters across conditions
ggplot(data=pd1) +
  geom_density(aes(x=estimate,fill=frame),alpha=0.5) +
  facet_grid(structure + model~parameter,scale="free")

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

# Individual Gradients

sg=posts %>%
  filter(source=="obs",parameter %in% c('w1','delta'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject,structure,model) %>%
  mutate(value = mean(value),
         d = (1:n())/n() )  %>% #hacky way to create different data points for different distances
  select(subject,frame,parameter,d,value,structure,model) %>%
  spread(key=parameter,value=value) %>%
  mutate( sg = (delta>=0)*w1*d^delta + (delta<0)*w1*(1-d^-delta))

ggplot(data=sg) +
  geom_line(aes(x=d,y=sg,group=subject),alpha=0.2) +
  facet_grid(structure + model~frame) + labs(x='distance',y='spatial gradient')

tg=posts %>%
  filter(source=="obs",parameter %in% c('w2','tau'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject,structure,model) %>%
  mutate(value = mean(value),
         t = (1:n())/n() ) %>%
  select(subject,frame,t,parameter,value,structure,model) %>%
  spread(key=parameter,value=value) %>%
  mutate( tg = (tau>=0)*w2*t^tau + (tau<0)*w2*(1-t^-tau))

ggplot(data=tg) +
  geom_line(aes(x=t,y=tg,group=subject),alpha=0.2) +
  facet_grid(structure + model~frame) + labs(x='time to deadline',y='temporal gradient')

stg=posts %>%
  filter(source=="obs",parameter %in% c('w3','alpha'),!is.na(parameter)) %>%
  group_by(parameter,frame,subject,structure,model) %>%
  mutate(value = mean(value),
         tod =  (1:n())/ (n()/3) ) %>%
  select(subject,frame,tod,parameter,value,structure,model) %>%
  spread(key=parameter,value=value) %>%
  mutate( max = 2*(1-alpha)*sqrt(alpha/(1-alpha)),
          stg = w3*max / (alpha*tod + (1-alpha)/tod ) )

ggplot(data=stg) +
  geom_line(aes(x=tod,y=stg,group=subject),alpha=0.2) +
  facet_grid(structure + model~frame) + labs(x='time to deadline / distance to goal',y='spatiotemporal gradient')

# tod: 1 = distance = deadline
# tod: if function peaks at less than 1, mostly decreases with expectancy
# tod: if function peaks at greater than 1, mostly increases with expectancy

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
  #group_by(parameter,frame,subject) %>%
  #summarise(value = mean(value)) %>%
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

ggplot(data=gradients,aes(x=d,y=t)) +
  geom_raster(aes(fill=g)) +
  geom_contour(aes(z=g),colour='gray50',binwidth=0.5) +
  scale_fill_distiller(palette="Spectral") + #,limits=c(0,20)) +
  facet_grid(~frame)

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


