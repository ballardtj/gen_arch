
rm(list=ls())

library(rstan)
library(tidyverse)
library(gridExtra)
library(grid)
library(foreach)
library(doMC)
registerDoMC(cores=7)


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

      pp_list[[ctr]]=pp_data_tmp
      #setTxtProgressBar(pb, ctr)
    }
    pp_data=bind_rows(pp_list)
  }

  return(pp_data)
}

generate_pp_plot=function(pp_data){

  pp_plot_data = pp_data %>%
    #Get proportion for each trial
    group_by(s,expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
    summarise(y_pred_tr = mean(y_pred),
              y_obs_tr = mean(y_obs)) %>%
    #Get proportion for each condition
    group_by(expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
    summarise(y_pred_con = mean(y_pred_tr),
              y_obs_con = mean(y_obs_tr),
              y_obs_con_se = sd(y_obs_tr)/sqrt(length(y_obs_tr))) %>%
    #Get posterior
    group_by(expt,a_d0,b_d0,a_t0,b_t0) %>%
    summarise(y_pred_mean = mean(y_pred_con),
              y_pred_hi = quantile(y_pred_con,0.975),
              y_pred_lo = quantile(y_pred_con,0.025),
              y_obs_mean = mean(y_obs_con),
              y_obs_hi = y_obs_mean + mean(y_obs_con_se),
              y_obs_lo = y_obs_mean - mean(y_obs_con_se))


  ds_pp = ggplot(data=subset(pp_plot_data,expt==1),aes(x=factor(a_d0),group=1)) +
    geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
    geom_line(aes(y=y_pred_mean),col="blue") +
    geom_line(aes(y=y_obs_mean),col="red") +
    #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
    facet_grid(.~factor(b_d0,labels=paste("Left Distance:",levels(factor(b_d0))))) +
    coord_cartesian(ylim=c(0,1)) +
    labs(x="Right Starting Distance",y=" ") +
    theme(legend.position="none")

  dl_pp =ggplot(data=subset(pp_plot_data,expt==2),aes(x=factor(a_t0),group=1)) +
    geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
    geom_line(aes(y=y_pred_mean),col="blue") +
    geom_line(aes(y=y_obs_mean),col="red") +
    #geom_errorbar(aes(ymin=y_obs_lo,ymax=y_obs_hi,col="red")) +
    facet_grid(.~factor(b_t0,labels=paste("Left Deadline:",levels(factor(b_t0))))) +
    coord_cartesian(ylim=c(0,1)) +
    labs(x="Right Deadline",y=" ") +
    theme(legend.position="none")

  dsdl_pp = pp_plot_data %>%
    filter(expt==3) %>%
    ungroup() %>%
    mutate(a_t0 = factor(a_t0,labels=paste("Right:",c(1,2,4,8),c("month","months","months","months"))),
           b_t0 = factor(b_t0,labels=paste("Left:",c(1,2,4,8),c("month","months","months","months")))) %>%
    ggplot(data=,aes(x=factor(200-a_d0),colour=factor(200-b_d0))) +
    #geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
    geom_point(aes(y=y_pred_mean),position=position_dodge(width=0.5)) +
    geom_errorbar(aes(ymin=y_pred_lo,ymax=y_pred_hi),width=0.1,position=position_dodge(width=0.5)) +
    geom_point(aes(y=y_obs_mean),shape=2,position=position_dodge(width=0.5)) +
    facet_grid(b_t0~a_t0) +
    coord_cartesian(ylim=c(0,1)) +
    labs(x="Right Starting Height (cm)",y=" ",colour="Left Starting Height (cm)") +
    theme(legend.position="bottom")


  return(list(ds_pp,dl_pp,dsdl_pp))
}


generate_trial_plot=function(ap_pp_data,av_pp_data){

  ap_pp_data_tmp = mutate(ap_pp_data,frame="Approach")
  av_pp_data_tmp = mutate(av_pp_data,frame="Avoidance")

  plot_data_tmp = bind_rows(ap_pp_data_tmp,av_pp_data_tmp) %>%
    #Get proportion for each trial
    group_by(s,expt,frame,a_d0,b_d0,a_t0,b_t0,sample) %>%
    summarise(y_pred_tr = mean(y_pred),
              y_obs_tr = mean(y_obs)) %>%
    #Get proportion for each condition
    group_by(expt,frame,a_d0,b_d0,a_t0,b_t0,sample) %>%
    summarise(y_pred_con = mean(y_pred_tr),
              y_obs_con = mean(y_obs_tr),
              y_obs_con_se = sd(y_obs_tr)/sqrt(length(y_obs_tr)))

    #Get CIs
  plot_data = plot_data_tmp
    group_by(expt,frame,a_d0,b_d0,a_t0,b_t0) %>%
    summarise(y_pred_mean = mean(y_pred_con),
              y_pred_hi = quantile(y_pred_con,0.975),
              y_pred_lo = quantile(y_pred_con,0.025),
              y_obs_mean = mean(y_obs_con))

    ggplot(plot_data) +
    geom_point(aes(x=y_obs_mean,y=y_pred_mean,colour=frame,shape=factor(expt)),alpha=0.5) +
   # geom_errorbar(aes(x=y_obs_mean,ymin=y_pred_lo,ymax=y_pred_hi,colour=frame),alpha=0.1) +
    geom_abline() +
    coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
    labs(x="Observed Condition Mean",y="Predicted Condition Mean",colour="Goal Type",shape="Experiment")

    ggsave(file=paste0("figures/predictives_scatterplot.png"),width=7,height=6)

    #Get CI on correlation between observed and predicted
    plot_data_tmp %>%
      group_by(sample) %>%
      summarise(cor = cor(y_pred_con,y_obs_con)) %>%
      ungroup() %>%
      summarise(min_cor = quantile(cor,0.025),
             max_cor = quantile(cor,0.975))

}



# source_labels=c(obs="Observed Decisions",opt="Optimal Decisions")
# structure_labels=c(hier="Hierarchical Model",fixed="Non-hierarchical Model")
# model_labels=c(space="Spatiotemporal Gradient Included",nospace='Spatiotemporal Gradient Omitted')


#load datalists
ap_dataList=read_rdump(paste0('data/clean/obs_ap_rdump_expt123.R'))  #Approach
av_dataList=read_rdump(paste0('data/clean/obs_av_rdump_expt123.R'))  #Avoidance


#Approach
load("data/derived/expt123_ap_fit.RData")
ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100)
ap_pp_plot= generate_pp_plot(ap_pp_data)

#Avoidance
load("data/derived/expt123_av_fit.RData")
av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100)
av_pp_plot= generate_pp_plot(av_pp_data)

#Posterior predictives
pp_fig = arrangeGrob(
  arrangeGrob(ap_pp_plot[[1]] + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 1, Approach Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(av_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 1, Avoidance Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(ap_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 2, Approach Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(av_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 2, Avoidance Condition")),gp=gpar(fontsize=12))),
  nrow=4,
  left="Proportion Prioritizing Right-hand Goal"
)

ggsave(file=paste0("figures/predictives_expt12.png"),plot=pp_fig,width=11,height=10)


#Posterior predictives
pp_fig = arrangeGrob(
  arrangeGrob(ap_pp_plot[[3]], #+ theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 3, Approach Condition")),gp=gpar(fontsize=12))),
  arrangeGrob(av_pp_plot[[3]], #  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
              top=textGrob(expression(italic("Experiment 3, Avoidance Condition")),gp=gpar(fontsize=12))),
  nrow=2,
  left="Proportion Prioritizing Right-hand Goal"
)

ggsave(file=paste0("figures/predictives_expt3.png"),plot=pp_fig,width=11,height=10)




# Observed Decisions
#
# ```{r observed_fits, eval=FALSE, fig.align="center", fig.height=12, fig.width=10, include=FALSE}
# #Approach
# #setwd("..")
# dataList=read_rdump('data/clean/obs_ap_rdump.R')
#
# load("data/derived/ap_obs_fit.RData")
# pp_data=generate_pp(fit=fit,dataList=dataList,Nsamp=10,source="obs")
# ap_obs_pp_plot= generate_pp_plot(pp_data)
#
#
# #Avoidance
# dataList=read_rdump('data/clean/obs_av_rdump.R')
#
# load("data/derived/av_obs_fit.RData")
# pp_data=generate_pp(fit=fit,dataList=dataList,Nsamp=10,source="obs")
# av_obs_pp_plot= generate_pp_plot(pp_data)
#
# #Posterior predictives
# grid.arrange(
#   arrangeGrob(ap_obs_pp_plot[[1]] + theme(axis.text.x = element_text(size=6)) ,
#               top="Distance Experiment, Approach Condition"),
#   arrangeGrob(av_obs_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Distance Experiment, Avoidance Condition"),
#   arrangeGrob(ap_obs_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Deadline Experiment, Approach Condition"),
#   arrangeGrob(av_obs_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Deadline Experiment, Avoidance Condition"),
#   nrow=4,height=12, width=10
# )
# ```
#
# # Optimal Decisions
#
# ```{r optimal_fits, eval=FALSE, fig.align="center", fig.height=12, fig.width=10, include=FALSE}
#
# #Approach
# #setwd("..")
# dataList=read_rdump('data/clean/opt_ap_rdump.R')
#
# load("data/derived/ap_opt_fit.RData")
# pp_data=generate_pp(fit=fit,dataList=dataList,Nsamp=100,source="opt")
# ap_opt_pp_plot= generate_pp_plot(pp_data)
#
#
# #Avoidance
# dataList=read_rdump('data/clean/opt_av_rdump.R')
#
# load("data/derived/av_opt_fit.RData")
# pp_data=generate_pp(fit=fit,dataList=dataList,Nsamp=100,source="opt")
# av_opt_pp_plot= generate_pp_plot(pp_data)
#
# #Posterior predictives
# grid.arrange(
#   arrangeGrob(ap_opt_pp_plot[[1]] + theme(axis.text.x = element_text(size=6)) ,
#               top="Distance Experiment, Approach Condition"),
#   arrangeGrob(av_opt_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Distance Experiment, Avoidance Condition"),
#   arrangeGrob(ap_opt_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Deadline Experiment, Approach Condition"),
#   arrangeGrob(av_opt_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6)) ,
#               top="Deadline Experiment, Avoidance Condition"),
#   nrow=4
# )
# ```
