

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

       # dataList=read_rdump(paste0('data/clean/',source,'_',frame,'_rdump.R'))
        dataList=read_rdump(paste0('data/clean/obs_',frame,'_rdump_expt3.R'))

        load(paste0("data/derived/expt3_",frame,"_fit.RData"))


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
sg1 %>% mutate(positive = delta >= 0) %>% count(frame,positive)

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

ggsave(file=paste0("figures/gradients_expt3.png"),plot=gradient_fig,width=6,height=9)






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


