
rm(list=ls())

library(rstan)
library(tidyverse)
library(gridExtra)
library(grid)
library(foreach)
library(doMC)
registerDoMC(cores=3)


#function to generate posterior predictives
generate_pp=function(fit,dataList,Nsamp){

  #get indicies of samples
  posts=rstan::extract(fit)
  #posts_obs=rstan:extract(fit_opt)
  Npost=dim(posts$w1)[1]
  samples=sample(x=1:Npost,size=Nsamp)

  pp_data <- foreach(i = 1:Nsamp,.combine='rbind') %dopar% {
    pp_list=list()
    ctr=0
    for(subj in 1:dataList$Nsubj){
      ctr=ctr+1

      #get number of valid observations for that subject
      Nvalid = dataList$int_data[subj,1];
      Nplaces = dataList$int_data[subj,2]; #total number of elements in array (including padding)

      #initialize gradient parameters
      delta_sub = posts$theta[samples[i],subj,4];
      tau_sub = posts$theta[samples[i],subj,5];
      alpha_sub = posts$theta[samples[i],subj,6];
      one_m_alpha_sub = 1 - alpha_sub;

      #uncenter and fill weight parameters
      dat = matrix(NA,nrow=3,ncol=Nvalid)
      weights = matrix(NA,nrow=1,ncol=3)
      weights[1] = posts$phi[samples[i],1] + posts$phi[samples[i],2]*posts$theta[samples[i],subj,1] #w1
      weights[2] = posts$phi[samples[i],3] + posts$phi[samples[i],4]*posts$theta[samples[i],subj,2] #w2
      weights[3] = (posts$phi[samples[i],5] + posts$phi[samples[i],6]*posts$theta[samples[i],subj,3])*2*one_m_alpha_sub*sqrt(alpha_sub / one_m_alpha_sub); #w3*max_spatial

      #calculate gradients
      a_sg = exp(dataList$real_data[subj,1:Nvalid]*delta_sub);  #implies a_d ^ delta
      b_sg = exp(dataList$real_data[subj,(Nplaces+1):(Nplaces+Nvalid)]*delta_sub); #implies b_d ^ delta
      dat[1,] = a_sg-b_sg;

      #calculate difference in temporal gradient
      a_tg = exp(dataList$real_data[subj,(2*Nplaces+1):(2*Nplaces+Nvalid)]*tau_sub); #implies a_t ^ tau
      b_tg = exp(dataList$real_data[subj,(3*Nplaces+1):(3*Nplaces+Nvalid)]*tau_sub); #implies b_t ^ tau
      dat[2,] = a_tg-b_tg;

      #calculate difference in spatiotemporal gradient
      a_stg_tmp = alpha_sub * dataList$real_data[subj,(6*Nplaces+1):(6*Nplaces+Nvalid)] + one_m_alpha_sub * dataList$real_data[subj,(4*Nplaces+1):(4*Nplaces+Nvalid)];
      b_stg_tmp = alpha_sub * dataList$real_data[subj,(7*Nplaces+1):(7*Nplaces+Nvalid)] + one_m_alpha_sub * dataList$real_data[subj,(5*Nplaces+1):(5*Nplaces+Nvalid)];
      a_stg = 1 / a_stg_tmp;
      b_stg = 1 / b_stg_tmp;
      dat[3,] = a_stg - b_stg;

      #matrix multiplication to calculate the logit for each observation for that subject
      p_a_logit = weights %*% dat;

      pp_data_tmp = data.frame(y_pred=rep(NA,dataList$Nobs[subj]),
                               y_obs=NA,
                               a_d0=NA,
                               b_d0=NA,
                               a_t0=NA,
                               b_t0=NA,
                               s = subj,
                               expt = dataList$expt[subj],
                               sample=samples[i])

      y_prob = as.vector(1/(1+exp(-p_a_logit)))
      pp_data_tmp$y_pred = 1*(y_prob > runif(n=length(y_prob)))
      pp_data_tmp$y_obs = dataList$y[1:dataList$Nobs[subj],subj]
      pp_data_tmp$a_d0 = dataList$a_d0[1:dataList$Nobs[subj],subj]
      pp_data_tmp$b_d0 = dataList$b_d0[1:dataList$Nobs[subj],subj]
      pp_data_tmp$a_t0 = dataList$a_t0[1:dataList$Nobs[subj],subj]
      pp_data_tmp$b_t0 = dataList$b_t0[1:dataList$Nobs[subj],subj]
      pp_data_tmp$a_d = exp(dataList$a_logd[1:dataList$Nobs[subj],subj])
      pp_data_tmp$b_d = exp(dataList$b_logd[1:dataList$Nobs[subj],subj])
      pp_data_tmp$a_t = exp(dataList$a_logt[1:dataList$Nobs[subj],subj])
      pp_data_tmp$b_t = exp(dataList$b_logt[1:dataList$Nobs[subj],subj])

      pp_list[[ctr]]=pp_data_tmp
      #setTxtProgressBar(pb, ctr)
    }
    pp_data=bind_rows(pp_list)
  }

  return(pp_data)
}

#theme
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


#load datalists
ap_dataList=read_rdump(paste0('data/clean/obs_ap_rdump_expt123.R'))  #Approach
av_dataList=read_rdump(paste0('data/clean/obs_av_rdump_expt123.R'))  #Avoidance

#Approach
load("data/derived/expt123_obs_ap_fit.RData")
ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100)

#Avoidance
load("data/derived/expt123_obs_av_fit.RData")
av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100)

ap_pp_data_tmp = mutate(ap_pp_data,goal_type="Approach")
av_pp_data_tmp = mutate(av_pp_data,goal_type="Avoidance")

plot_data_tmp = bind_rows(ap_pp_data_tmp,av_pp_data_tmp) %>%
  #Get proportion for each trial
  group_by(s,expt,goal_type,a_d0,b_d0,a_t0,b_t0,sample) %>%
  summarise(y_pred_tr = mean(y_pred),
            y_obs_tr = mean(y_obs)) %>%
  #Get proportion for each condition
  group_by(expt,goal_type,a_d0,b_d0,a_t0,b_t0,sample) %>%
  summarise(y_pred_con = mean(y_pred_tr),
            y_obs_con = mean(y_obs_tr))

#Get CIs
plot_data = plot_data_tmp %>%
  group_by(expt,goal_type,a_d0,b_d0,a_t0,b_t0) %>%
  summarise(y_pred_mean = mean(y_pred_con),
            y_obs_mean = mean(y_obs_con))

ggplot(plot_data) +
  geom_point(aes(x=y_obs_mean,y=y_pred_mean,colour=goal_type,shape=factor(expt)),alpha=0.5) +
  geom_abline() +
  coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
  labs(x="Observed Condition Mean",y="Predicted Condition Mean",colour="Goal Type",shape="Experiment") +
  scale_colour_manual(values=c("blue","red")) +
  theme.goal

ggsave(file=paste0("figures/predictives_scatterplot.png"),width=6,height=5)

#Get CI on correlation between observed and predicted
plot_data_tmp %>%
  group_by(sample) %>%
  summarise(cor = cor(y_pred_con,y_obs_con)) %>%
  ungroup() %>%
  summarise(min_cor = quantile(cor,0.025),
            max_cor = quantile(cor,0.975))



#function to generate vector fields with posterior predictives super imposed (INCOMPLETE, PROBABLY NOT NEEDED)
# pp_data_vector_plot = function(data,min_n,goal_type_to_plot){
#
#   if(goal_type_to_plot=="Approach"){
#     data_color = "blue"
#     pp_color = "skyblue"
#     }
#   if(goal_type_to_plot=="Avoidance"){
#     data_color = "red"
#     pp_color = "pink"
#     }
#
#   pd = data %>% mutate(
#     #bin deadline variable into three categories to facilitate plotting
#     right_deadline_bin = case_when(
#       a_t < 0.25 ~ 1,
#       a_t>=0.25 & a_t<0.5 ~ 2,
#       a_t >= 0.5 ~ 3),
#     left_deadline_bin = case_when(
#       b_t < 0.25 ~ 1,
#       b_t>=0.25 & b_t<0.5 ~ 2,
#       b_t >= 0.5 ~ 3),
#     #bin distance variable by rounding to nearest 10 cm increment.
#     left_current_distance_bin = round(b_d*10)/10,
#     right_current_distance_bin = round(a_d*10)/10) %>%
#     #Get proportion for each bin
#     group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type,sample) %>%
#     summarise(y_pred_tmp = mean(y_pred),
#               y_obs_tmp = mean(y_obs),
#               n = n()) %>%
#     filter(n >= min_n) %>%
#     #Get posterior
#     group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type) %>%
#     summarise(y_pred_mean = mean(y_pred_tmp),
#               y_pred_hi = quantile(y_pred_tmp,0.975),
#               y_pred_lo = quantile(y_pred_tmp,0.025),
#               y_obs_mean = mean(y_obs_tmp),
#               xend_obs = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_obs_mean*0.1 - (goal_type[1]=="Avoidance")*(1-y_obs_mean)*0.1 ,
#               yend_obs = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_obs_mean)*0.1 - (goal_type[1]=="Avoidance")*y_obs_mean*0.1 ,
#               xend_pred_mean = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_mean*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_mean)*0.1,
#               yend_pred_mean = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_mean  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_mean*0.1,
#               xend_pred_lo = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_lo*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_lo)*0.1,
#               yend_pred_lo = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_lo  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_lo*0.1,
#               xend_pred_hi = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_hi*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_hi)*0.1,
#               yend_pred_hi = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_hi  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_hi*0.1)
#
#
#   pd1 = pd %>% ungroup() %>% mutate(group = 1,polygroup = 1:n())
#   pd2 = pd %>% ungroup() %>% mutate(group = 2,polygroup = 1:n())
#   pd3 = pd %>% ungroup() %>% mutate(group = 3,polygroup = 1:n())
#
#   pd_poly = bind_rows(pd1,pd2,pd3) %>%
#     mutate(poly_x = case_when(
#       group == 1 ~ right_current_distance_bin,
#       group == 2 ~ xend_pred_hi,
#       group == 3 ~ xend_pred_lo
#     ),
#     poly_y = case_when(
#       group == 1 ~ left_current_distance_bin,
#       group == 2 ~ yend_pred_hi,
#       group == 3 ~ yend_pred_lo
#     )
#     )
#
#   ggplot( data = pd ,
#           aes(x = right_current_distance_bin, y = left_current_distance_bin)) +
#     # geom_raster() +
#     facet_grid(goal_type + left_deadline_bin ~ right_deadline_bin ) +
#
#     geom_polygon(data=pd_poly,aes(x=poly_x,y=poly_y,group=polygroup),fill=pp_color) +
#     ##geom_point(size=0.5,col="red",aes(x=xend_pred_lo,y=yend_pred_hi)) +
#     #geom_segment(aes(xend=xend_pred_mean,yend=yend_pred_mean),arrow = arrow(length = unit(0.02, "npc")),alpha=1,col="blue") +
#     geom_segment(aes(xend=xend_obs,yend=yend_obs),arrow = arrow(length = unit(0.02, "npc")),alpha=1,col=data_color) +
#     ylab('Left Distance to Goal (D)') +
#     xlab("Right Distance to Goal (D)") +
#     # theme.goal +
#     #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
#     #scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
#     labs(fill = "Proportion Prioritizing\nRight-hand Crop/Weed") +
#     scale_x_reverse() +
#     scale_y_reverse() +
#     geom_vline(xintercept=0,size=2.5) +
#     geom_hline(yintercept=0,size=2.5) +
#     geom_vline(xintercept=0,size=0.5,color='yellow') +
#     geom_hline(yintercept=0,size=0.5,color='yellow') +
#     ggtitle(goal_type_to_plot)
#
#




#   data %>%
#     filter(n >= min_n,
#            source == source_to_plot,
#            goal_type == goal_type_to_plot) %>%
#
#     ggplot( aes(y = left_current_distance_bin, x = right_current_distance_bin, fill = choice_right,color=choice_right)) +
#     # geom_raster() +
#     geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.02, "npc")),alpha=1 ) +
#     facet_grid(left_start_deadlineF ~ right_start_deadlineF ) +
#     ylab('Left Distance to Goal (D)') +
#     xlab("Right Distance to Goal (D)") +
#     theme.goal +
#     #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
#     scale_colour_distiller(palette = vector_color, limits = c(0, 1)) +
#     labs(fill = "Proportion Prioritizing\nRight-hand Crop/Weed") +
#     scale_x_reverse() +
#     scale_y_reverse() +
#     geom_vline(xintercept=0,size=2.5) +
#     geom_hline(yintercept=0,size=2.5) +
#     geom_vline(xintercept=0,size=0.5,color='yellow') +
#     geom_hline(yintercept=0,size=0.5,color='yellow') +
#     theme(legend.position = "none") +
#     ggtitle(goal_type_to_plot)
#
# }
#
#
#
# #load datalists
# ap_dataList=read_rdump(paste0('data/clean/obs_ap_rdump_expt123.R'))  #Approach
# av_dataList=read_rdump(paste0('data/clean/obs_av_rdump_expt123.R'))  #Avoidance
#
# #load fit objects
# load("data/derived/expt123_ap_fit.RData")
# load("data/derived/expt123_av_fit.RData")
#
# #generate posterior predictives
# ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100)  %>% mutate(goal_type="Approach")
# av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100)  %>% mutate(goal_type="Avoidance")
#
# #get formatted data for scatter plot
# ap_scatter_data = get_scatter_data(ap_pp_data)
# av_scatter_data = get_scatter_data(av_pp_data)
# scatter_data = bind_rows(ap_scatter_data,av_scatter_data)
#
# #generate scatter plot
# generate_scatter_plot(ap_pp_data,av_pp_data)
#
#
# ### POSTERIOR PREDICTIVES SUPERIMPOSED ONTO VECTOR FIELDS
#
# pd = bind_rows(ap_pp_data,av_pp_data) %>%
#   #calculate bins for distance and deadline
#   mutate(
#           #bin deadline variable into three categories to facilitate plotting
#           right_deadline_bin = case_when(
#              a_t < 0.25 ~ 1,
#              a_t>=0.25 & a_t<0.5 ~ 2,
#              a_t >= 0.5 ~ 3),
#           left_deadline_bin = case_when(
#              b_t < 0.25 ~ 1,
#              b_t>=0.25 & b_t<0.5 ~ 2,
#              b_t >= 0.5 ~ 3),
#           #bin distance variable by rounding to nearest 10 cm increment.
#           left_current_distance_bin = round(b_d*10)/10,
#           right_current_distance_bin = round(a_d*10)/10) %>%
#   #Get proportion for each bin
#   group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type,sample) %>%
#   summarise(y_pred_tmp = mean(y_pred),
#             y_obs_tmp = mean(y_obs),
#             n = n()) %>%
#   #Get posterior
#   group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type) %>%
#   summarise(n = mean(n),
#             y_pred_mean = mean(y_pred_tmp),
#             y_pred_hi = quantile(y_pred_tmp,0.975),
#             y_pred_lo = quantile(y_pred_tmp,0.025),
#             y_obs_mean = mean(y_obs_tmp),
#             xend_obs = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_obs_mean*0.1 - (goal_type[1]=="Avoidance")*(1-y_obs_mean)*0.1 ,
#             yend_obs = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_obs_mean)*0.1 - (goal_type[1]=="Avoidance")*y_obs_mean*0.1 ,
#             xend_pred_mean = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_mean*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_mean)*0.1,
#             yend_pred_mean = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_mean  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_mean*0.1,
#             xend_pred_lo = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_lo*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_lo)*0.1,
#             yend_pred_lo = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_lo  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_lo*0.1,
#             xend_pred_hi = mean(right_current_distance_bin) - (goal_type[1]=="Approach")*y_pred_hi*0.1 - (goal_type[1]=="Avoidance")*(1-y_pred_hi)*0.1,
#             yend_pred_hi = mean(left_current_distance_bin) - (goal_type[1]=="Approach")*(1-y_pred_hi  )*0.1 - (goal_type[1]=="Avoidance")*y_pred_hi*0.1)
#
#
# pd1 = pd %>% ungroup() %>% mutate(group = 1,polygroup = 1:n())
# pd2 = pd %>% ungroup() %>% mutate(group = 2,polygroup = 1:n())
# pd3 = pd %>% ungroup() %>% mutate(group = 3,polygroup = 1:n())
#
# pd_poly = bind_rows(pd1,pd2,pd3) %>%
#   mutate(poly_x = case_when(
#     group == 1 ~ right_current_distance_bin,
#     group == 2 ~ xend_pred_hi,
#     group == 3 ~ xend_pred_lo
#   ),
#   poly_y = case_when(
#     group == 1 ~ left_current_distance_bin,
#     group == 2 ~ yend_pred_hi,
#     group == 3 ~ yend_pred_lo
#   )
#   )
#
#
#
#
#
#
#
# ap_pp_plot= generate_pp_plot(ap_pp_data)
#
# #Avoidance
#
#
# av_pp_plot= generate_pp_plot(av_pp_data)
#
#
#
#
# generate_pp_plot_scatter_by_cell =function(ap_pp_data){
#
#   pp_plot_data = pp_data %>%
#     mutate(
#     #bin deadline variable into three categories to facilitate plotting
#     right_deadline_bin = case_when(
#       a_t < 0.25 ~ 1,
#       a_t>=0.25 & a_t<0.5 ~ 2,
#       a_t >= 0.5 ~ 3),
#     left_deadline_bin = case_when(
#       b_t < 0.25 ~ 1,
#       b_t>=0.25 & b_t<0.5 ~ 2,
#       b_t >= 0.5 ~ 3),
#     #bin distance variable by rounding to nearest 10 cm increment.
#     left_current_distance_bin = round(b_d*10)/10,
#     right_current_distance_bin = round(a_d*10)/10) %>%
#   #summarise choice proportion for each combination of distance, deadline, and goal type.
#   group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin,sample) %>%
#   summarise(  n = n(),
#               y_pred_m = mean(y_pred),      #predicted choices - mean across subjects and cells
#               y_obs_m = mean(y_obs) ) %>%   #observed choices - mean across subject and cells
#   #aggregate choice proportions across samples to get posterior predictive distribution
#   group_by(left_deadline_bin,right_deadline_bin,left_current_distance_bin,right_current_distance_bin) %>%
#     summarise( y_obs_m = mean(y_obs_m),
#                y_pred_lo = quantile(y_pred_m,0.025),
#                y_pred_hi = quantile(y_pred_m,0.975),
#                y_pred_mid = mean(y_pred_m)) %>%
#     ggplot() +
#     geom_point(aes(x=))
#
#   ds_pp = ggplot(data=subset(pp_plot_data,expt==1),aes(x=factor(a_d0),group=1)) +
#     geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_line(aes(y=y_pred_mean),col="blue") +
#     geom_line(aes(y=y_obs_mean),col="red") +
#     #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
#     facet_grid(.~factor(b_d0,labels=paste("Left Distance:",levels(factor(b_d0))))) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Starting Distance",y=" ") +
#     theme(legend.position="none")
#
#   dl_pp =ggplot(data=subset(pp_plot_data,expt==2),aes(x=factor(a_t0),group=1)) +
#     geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_line(aes(y=y_pred_mean),col="blue") +
#     geom_line(aes(y=y_obs_mean),col="red") +
#     #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
#     facet_grid(.~factor(b_t0,labels=paste("Left Deadline:",levels(factor(b_t0))))) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Deadline",y=" ") +
#     theme(legend.position="none")
#
#   dsdl_pp = pp_plot_data %>%
#     filter(expt==3) %>%
#     ungroup() %>%
#     mutate(a_t0 = factor(a_t0,labels=paste("Right:",c(1,2,4,8),c("month","months","months","months"))),
#            b_t0 = factor(b_t0,labels=paste("Left:",c(1,2,4,8),c("month","months","months","months")))) %>%
#     ggplot(data=,aes(x=factor(200-a_d0),colour=factor(200-b_d0))) +
#     #geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_point(aes(y=y_pred_mean),position=position_dodge(width=0.5)) +
#     geom_errorbar(aes(ymin=y_pred_lo,ymax=y_pred_hi),width=0.1,position=position_dodge(width=0.5)) +
#     geom_point(aes(y=y_obs_mean),shape=2,position=position_dodge(width=0.5)) +
#     facet_grid(b_t0~a_t0) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Starting Height (cm)",y=" ",colour="Left Starting Height (cm)") +
#     theme(legend.position="bottom")
#
#
#   return(list(ds_pp,dl_pp,dsdl_pp))
# }
#
#
# generate_pp_plot=function(pp_data){
#
#   pp_plot_data = pp_data %>%
#     #Get proportion for each trial
#     group_by(s,expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
#     summarise(y_pred_tr = mean(y_pred),
#               y_obs_tr = mean(y_obs)) %>%
#     #Get proportion for each condition
#     group_by(expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
#     summarise(y_pred_con = mean(y_pred_tr),
#               y_obs_con = mean(y_obs_tr),
#               y_obs_con_se = sd(y_obs_tr)/sqrt(length(y_obs_tr))) %>%
#     #Get posterior
#     group_by(expt,a_d0,b_d0,a_t0,b_t0) %>%
#     summarise(y_pred_mean = mean(y_pred_con),
#               y_pred_hi = quantile(y_pred_con,0.975),
#               y_pred_lo = quantile(y_pred_con,0.025),
#               y_obs_mean = mean(y_obs_con),
#               y_obs_hi = y_obs_mean + mean(y_obs_con_se),
#               y_obs_lo = y_obs_mean - mean(y_obs_con_se))
#
#
#   ds_pp = ggplot(data=subset(pp_plot_data,expt==1),aes(x=factor(a_d0),group=1)) +
#     geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_line(aes(y=y_pred_mean),col="blue") +
#     geom_line(aes(y=y_obs_mean),col="red") +
#     #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
#     facet_grid(.~factor(b_d0,labels=paste("Left Distance:",levels(factor(b_d0))))) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Starting Distance",y=" ") +
#     theme(legend.position="none")
#
#   dl_pp =ggplot(data=subset(pp_plot_data,expt==2),aes(x=factor(a_t0),group=1)) +
#     geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_line(aes(y=y_pred_mean),col="blue") +
#     geom_line(aes(y=y_obs_mean),col="red") +
#     #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
#     facet_grid(.~factor(b_t0,labels=paste("Left Deadline:",levels(factor(b_t0))))) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Deadline",y=" ") +
#     theme(legend.position="none")
#
#   dsdl_pp = pp_plot_data %>%
#     filter(expt==3) %>%
#     ungroup() %>%
#     mutate(a_t0 = factor(a_t0,labels=paste("Right:",c(1,2,4,8),c("month","months","months","months"))),
#            b_t0 = factor(b_t0,labels=paste("Left:",c(1,2,4,8),c("month","months","months","months")))) %>%
#     ggplot(data=,aes(x=factor(200-a_d0),colour=factor(200-b_d0))) +
#     #geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
#     geom_point(aes(y=y_pred_mean),position=position_dodge(width=0.5)) +
#     geom_errorbar(aes(ymin=y_pred_lo,ymax=y_pred_hi),width=0.1,position=position_dodge(width=0.5)) +
#     geom_point(aes(y=y_obs_mean),shape=2,position=position_dodge(width=0.5)) +
#     facet_grid(b_t0~a_t0) +
#     coord_cartesian(ylim=c(0,1)) +
#     labs(x="Right Starting Height (cm)",y=" ",colour="Left Starting Height (cm)") +
#     theme(legend.position="bottom")
#
#
#   return(list(ds_pp,dl_pp,dsdl_pp))
# }
#
#
#
#
#
# # source_labels=c(obs="Observed Decisions",opt="Optimal Decisions")
# # structure_labels=c(hier="Hierarchical Model",fixed="Non-hierarchical Model")
# # model_labels=c(space="Spatiotemporal Gradient Included",nospace='Spatiotemporal Gradient Omitted')
#
#
# #load datalists
# ap_dataList=read_rdump(paste0('data/clean/obs_ap_rdump_expt123.R'))  #Approach
# av_dataList=read_rdump(paste0('data/clean/obs_av_rdump_expt123.R'))  #Avoidance
#
# #Approach
# load("data/derived/expt123_ap_fit.RData")
# ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100)
# ap_pp_plot= generate_pp_plot(ap_pp_data)
#
# #Avoidance
# load("data/derived/expt123_av_fit.RData")
# av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100)
# av_pp_plot= generate_pp_plot(av_pp_data)
#
# #Posterior predictives
# pp_fig = arrangeGrob(
#   arrangeGrob(ap_pp_plot[[1]] + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 1, Approach Condition")),gp=gpar(fontsize=12))),
#   arrangeGrob(av_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 1, Avoidance Condition")),gp=gpar(fontsize=12))),
#   arrangeGrob(ap_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 2, Approach Condition")),gp=gpar(fontsize=12))),
#   arrangeGrob(av_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 2, Avoidance Condition")),gp=gpar(fontsize=12))),
#   nrow=4,
#   left="Proportion Prioritizing Right-hand Goal"
# )
#
# ggsave(file=paste0("figures/predictives_expt12.png"),plot=pp_fig,width=11,height=10)
#
#
# #Posterior predictives
# pp_fig = arrangeGrob(
#   arrangeGrob(ap_pp_plot[[3]], #+ theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 3, Approach Condition")),gp=gpar(fontsize=12))),
#   arrangeGrob(av_pp_plot[[3]], #  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
#               top=textGrob(expression(italic("Experiment 3, Avoidance Condition")),gp=gpar(fontsize=12))),
#   nrow=2,
#   left="Proportion Prioritizing Right-hand Goal"
# )
#
# ggsave(file=paste0("figures/predictives_expt3.png"),plot=pp_fig,width=11,height=10)
#
#
# ### VECTOR FIELDS ###
#
# ap_vf = mutate(ap_pp_data,goal_type = "Approach") %>% filter(expt==3)
# av_vf = mutate(av_pp_data,goal_type = "Avoidance") %>% filter(expt==3)
#
# pd = bind_rows(ap_vf,av_vf) %>%
#   #calculate bins for distance and deadline
#   mutate(a_d_bin = round(a_d*188/15)*15,
#          b_d_bin = round(b_d*188/15)*15,
#          a_t0_bin = as.numeric(a_t0 > 2),
#          b_t0_bin = as.numeric(b_t0 > 2)) %>%
#   #Get proportion for each trial
#   group_by(s,expt,a_d_bin,b_d_bin,a_t0_bin,b_t0_bin,sample,goal_type) %>%
#   summarise(y_pred_tr = mean(y_pred),
#             y_obs_tr = mean(y_obs)) %>%
#   #Get proportion for each bin
#   group_by(expt,a_d_bin,b_d_bin,a_t0_bin,b_t0_bin,sample,goal_type) %>%
#   summarise(y_pred_con = mean(y_pred_tr),
#             y_obs_con = mean(y_obs_tr),
#             y_obs_con_se = sd(y_obs_tr)/sqrt(length(y_obs_tr))) %>%
#   #Get posterior
#   group_by(expt,a_d_bin,b_d_bin,a_t0_bin,b_t0_bin,goal_type) %>%
#   summarise(y_pred_mean = mean(y_pred_con),
#             y_pred_hi = quantile(y_pred_con,0.975),
#             y_pred_lo = quantile(y_pred_con,0.025),
#             y_obs_mean = mean(y_obs_con),
#             y_obs_hi = y_obs_mean + mean(y_obs_con_se),
#             y_obs_lo = y_obs_mean - mean(y_obs_con_se),
#             xend_obs = mean(a_d_bin) - y_obs_mean*10,
#             yend_obs = mean(b_d_bin) - (1-y_obs_mean)*10,
#             xend_pred_mean = mean(a_d_bin) - y_pred_mean*10,
#             yend_pred_mean = mean(b_d_bin) - (1-y_pred_mean)*10,
#             xend_pred_lo = mean(a_d_bin) - y_pred_lo*10,
#             yend_pred_lo = mean(b_d_bin) - (1-y_pred_lo)*10,
#             xend_pred_hi = mean(a_d_bin) - y_pred_hi*10,
#             yend_pred_hi = mean(b_d_bin) - (1-y_pred_hi)*10)
#
# pd1 = pd %>% ungroup() %>% mutate(group = 1,polygroup = 1:n())
# pd2 = pd %>% ungroup() %>% mutate(group = 2,polygroup = 1:n())
# pd3 = pd %>% ungroup() %>% mutate(group = 3,polygroup = 1:n())
#
# pd_poly = bind_rows(pd1,pd2,pd3) %>%
#   mutate(poly_x = case_when(
#     group == 1 ~ a_d_bin,
#     group == 2 ~ xend_pred_hi,
#     group == 3 ~ xend_pred_lo
#   ),
#   poly_y = case_when(
#     group == 1 ~ b_d_bin,
#     group == 2 ~ yend_pred_hi,
#     group == 3 ~ yend_pred_lo
#     )
#   )
#
# ggplot( data = pd ,
#         aes(x = a_d_bin, y = b_d_bin)) +
#   # geom_raster() +
#   facet_grid(goal_type + b_t0_bin ~ a_t0_bin ) +
#
#   geom_polygon(data=pd_poly,aes(x=poly_x,y=poly_y,group=polygroup),fill="skyblue") +
#  ##geom_point(size=0.5,col="red",aes(x=xend_pred_lo,y=yend_pred_hi)) +
#   geom_segment(aes(xend=xend_pred_mean,yend=yend_pred_mean),arrow = arrow(length = unit(0.02, "npc")),alpha=1,col="blue") +
#   geom_segment(aes(xend=xend_obs,yend=yend_obs),arrow = arrow(length = unit(0.02, "npc")),alpha=1,col="red") +
#
#
#   ylab('Left Starting Height (cm)') +
#   xlab("Right Starting Height (cm)") +
#  # theme.goal +
#   #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
#   scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
#   labs(fill = "Proportion Prioritizing\nRight-hand Crop/Weed") +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   geom_vline(xintercept=0,size=2.5) +
#   geom_hline(yintercept=0,size=2.5) +
#   geom_vline(xintercept=0,size=0.5,color='yellow') +
#   geom_hline(yintercept=0,size=0.5,color='yellow') +
#   coord_cartesian(ylim= c(-20,100),xlim=c(-20,100))
#
#
# #Implement theme from discriptives
# #Covert to 1 0 scale
# #Labels for facets
#
#
# pd <- data_bound %>%
#   filter(phase==1,expt==3, left_current_distance > 0 , right_current_distance > 0 )   %>%
#   mutate(left_start_deadline_bin = as.numeric(left_start_deadline > 2),
#          right_start_deadline_bin = as.numeric(right_start_deadline > 2),
#          left_current_distance_bin = round(left_current_distance/10)*10,
#          right_current_distance_bin = round(right_current_distance/10)*10) %>%
#   group_by(subject,trial_number,left_start_deadline_bin,right_start_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type ) %>%
#   summarise( prioritise_right = mean(prioritise_right),
#              policy_right = mean(policy)) %>%
#   group_by(left_start_deadline_bin,right_start_deadline_bin,left_current_distance_bin,right_current_distance_bin,goal_type) %>%
#   summarise(  prioritise_right = mean(prioritise_right),
#               policy_right = mean(policy_right) ) %>%
#   gather(source,choice_right,prioritise_right,policy_right) %>%
#   mutate(left_start_deadlineF = factor(left_start_deadline_bin,levels=0:1,labels=c("Left: 1 or 2 Months","Left: 4 or 8 Months")),
#          right_start_deadlineF = factor(right_start_deadline_bin,levels=0:1,labels=c("Right: 1 or 2 Months","Right: 4 or 8 Months"))) %>%
#   mutate(xend = right_current_distance_bin - choice_right*6,
#          yend = left_current_distance_bin - (1-choice_right  )*6 )
#
# #pd$choice_farther[pd$left_start_deadline==pd$right_start_deadline] = NA
#
# pd$source = factor(
#   pd$source,
#   levels = c('prioritise_right', 'policy_right'),
#   labels = c('Observed', 'Achievement Maximizing')
# )
#
# ggplot( data = subset(pd , source == "Achievement Maximizing"),
#         aes(y = left_current_distance_bin, x = right_current_distance_bin, fill = choice_right, color=choice_right)) +
#   # geom_raster() +
#   geom_segment(aes(xend=xend,yend=yend),arrow = arrow(length = unit(0.02, "npc")),alpha=1) +
#   facet_grid(goal_type + left_start_deadlineF ~ source + right_start_deadlineF ) +
#   ylab('Left Starting Height (cm)') +
#   xlab("Right Starting Height (cm)") +
#   theme.goal +
#   #scale_fill_gradient2(low="blue",high="red",mid="black",midpoint=0.5,na.value='black',limits=c(0,1)) +
#   scale_colour_distiller(palette = "Spectral", limits = c(0, 1)) +
#   labs(fill = "Proportion Prioritizing\nRight-hand Crop/Weed") +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   geom_vline(xintercept=0,size=2.5) +
#   geom_hline(yintercept=0,size=2.5) +
#   geom_vline(xintercept=0,size=0.5,color='yellow') +
#   geom_hline(yintercept=0,size=0.5,color='yellow') +
#   coord_cartesian(ylim= c(-20,100),xlim=c(-20,100))
#
#
#
