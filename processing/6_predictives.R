

library(rstan)
library(tidyverse)
library(gridExtra)
library(grid)
library(foreach)
library(doMC)
registerDoMC(cores=7)

#load stan function for generating predictions
expose_stan_functions("model/goal_hier_space.stan")
goal_sub_hier_space = goal_sub

# expose_stan_functions("model/goal_fixed_space.stan")
# goal_sub_fixed_space = goal_sub
#
# expose_stan_functions("model/goal_hier_nospace.stan")
# goal_sub_hier_nospace = goal_sub
#
# expose_stan_functions("model/goal_fixed_nospace.stan")
# goal_sub_fixed_nospace = goal_sub

#function to generate posterior predictives
generate_pp=function(fit,dataList,Nsamp,version){

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

      if(version=="hier_space"){
        #observed model run on observed decision points
        p_a_logit=goal_sub_hier_space(posts$w1_mean[samples[i]],
                                      posts$w1_sd[samples[i]],
                                      posts$w1[samples[i],],
                                      posts$w2_mean[samples[i]],
                                      posts$w2_sd[samples[i]],
                                      posts$w2[samples[i],],
                                      posts$w3_mean[samples[i]],
                                      posts$w3_sd[samples[i]],
                                      posts$w3[samples[i],],
                                      posts$delta[samples[i],],
                                      posts$tau[samples[i],] ,
                                      posts$alpha[samples[i],],
                                      dataList$Nobs,
                                      subj,
                                      dataList$expt,
                                      dataList$s1,
                                      dataList$s2,
                                      dataList$a_logd,
                                      dataList$b_logd,
                                      dataList$a_logt,
                                      dataList$b_logt,
                                      dataList$a_dot,
                                      dataList$b_dot,
                                      dataList$a_tod,
                                      dataList$b_tod)
      }

      if(version=="fixed_space"){
        #observed model run on observed decision points
        p_a_logit=goal_sub_fixed_space(posts$w1[samples[i],],
                                       posts$w2[samples[i],],
                                       posts$w3[samples[i],],
                                       posts$delta[samples[i],],
                                       posts$tau[samples[i],] ,
                                       posts$alpha[samples[i],],
                                       dataList$Nobs,
                                       subj,
                                       dataList$expt,
                                       dataList$s1,
                                       dataList$s2,
                                       dataList$a_logd,
                                       dataList$b_logd,
                                       dataList$a_logt,
                                       dataList$b_logt,
                                       dataList$a_dot,
                                       dataList$b_dot,
                                       dataList$a_tod,
                                       dataList$b_tod)
      }

      if(version=="hier_nospace"){
        #observed model run on observed decision points
        p_a_logit=goal_sub_hier_nospace(posts$w1_mean[samples[i]],
                                        posts$w1_sd[samples[i]],
                                        posts$w1[samples[i],],
                                        posts$w2_mean[samples[i]],
                                        posts$w2_sd[samples[i]],
                                        posts$w2[samples[i],],
                                        posts$delta[samples[i],],
                                        posts$tau[samples[i],] ,
                                        dataList$Nobs,
                                        subj,
                                        dataList$expt,
                                        dataList$s1,
                                        dataList$s2,
                                        dataList$a_logd,
                                        dataList$b_logd,
                                        dataList$a_logt,
                                        dataList$b_logt,
                                        dataList$a_dot,
                                        dataList$b_dot,
                                        dataList$a_tod,
                                        dataList$b_tod)
      }

      if(version=="fixed_nospace"){
        #observed model run on observed decision points
        p_a_logit=goal_sub_fixed_nospace(posts$w1[samples[i],],
                                         posts$w2[samples[i],],
                                         posts$delta[samples[i],],
                                         posts$tau[samples[i],] ,
                                         dataList$Nobs,
                                         subj,
                                         dataList$expt,
                                         dataList$s1,
                                         dataList$s2,
                                         dataList$a_logd,
                                         dataList$b_logd,
                                         dataList$a_logt,
                                         dataList$b_logt,
                                         dataList$a_dot,
                                         dataList$b_dot,
                                         dataList$a_tod,
                                         dataList$b_tod)
      }


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

  return(list(ds_pp,dl_pp))
}


generate_trial_plot=function(ap_pp_data,av_pp_data){


  ap_tr_plot_data = ap_pp_data %>%
    #Get proportion for each trial
    group_by(s,expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
    summarise(y_pred_tr = mean(y_pred),
              y_obs_tr = mean(y_obs)) %>%
    group_by(s,expt,a_d0,b_d0,a_t0,b_t0) %>%
    summarise(y_pred_mean = mean(y_pred_tr),
              y_pred_hi = quantile(y_pred_tr,0.975),
              y_pred_lo = quantile(y_pred_tr,0.025),
              y_obs_mean = mean(y_obs_tr)) %>%
    group_by(s) %>%
    mutate(trial = 1:n(),
           frame="Approach") %>%
    ungroup() %>%
    select(s,trial,expt,frame,y_pred_mean,y_pred_hi,y_pred_lo,y_obs_mean)

    av_tr_plot_data = av_pp_data %>%
      #Get proportion for each trial
      group_by(s,expt,a_d0,b_d0,a_t0,b_t0,sample) %>%
      summarise(y_pred_tr = mean(y_pred),
                y_obs_tr = mean(y_obs)) %>%
      group_by(s,expt,a_d0,b_d0,a_t0,b_t0) %>%
      summarise(y_pred_mean = mean(y_pred_tr),
                y_pred_hi = quantile(y_pred_tr,0.975),
                y_pred_lo = quantile(y_pred_tr,0.025),
                y_obs_mean = mean(y_obs_tr)) %>%
      group_by(s) %>%
      mutate(trial = 1:n(),frame="Avoidance") %>%
      ungroup() %>%
    select(s,trial,expt,frame,y_pred_mean,y_pred_hi,y_pred_lo,y_obs_mean)

   bind_rows(ap_tr_plot_data,av_tr_plot_data) %>%
     ggplot() +
     geom_point(aes(x=y_obs_mean,y=y_pred_mean,colour=frame,shape=factor(expt)),alpha=0.5) +
     #geom_errorbar(aes(x=y_obs_mean,ymin=y_pred_lo,ymax=y_pred_hi,colour=frame),alpha=0.5) +
     geom_abline() +
     labs(x="Observed Trial Mean",y="Predicted Trial Mean",colour="Goal Type",shape="Experiment")

}



# source_labels=c(obs="Observed Decisions",opt="Optimal Decisions")
# structure_labels=c(hier="Hierarchical Model",fixed="Non-hierarchical Model")
# model_labels=c(space="Spatiotemporal Gradient Included",nospace='Spatiotemporal Gradient Omitted')

for (source in c('obs')){#},'opt')){

  #load datalists
  ap_dataList=read_rdump(paste0('data/clean/',source,'_ap_rdump.R'))  #Approach
  av_dataList=read_rdump(paste0('data/clean/',source,'_av_rdump.R'))  #Avoidance

  for (structure in c('hier')){
    for (model in c('space')){

      #Approach
      load(paste0("data/derived/ap_",source,"_",structure,"_",model,"_fit.RData"))
      ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100,version=paste(structure,model,sep='_' ))
      ap_pp_plot= generate_pp_plot(ap_pp_data)

      #Avoidance
      load(paste0("data/derived/av_",source,"_",structure,"_",model,"_fit.RData"))
      av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100,version=paste(structure,model,sep='_' ))
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

      ggsave(file=paste0("figures/predictives_",source,"_",structure,"_",model,".png"),plot=pp_fig,width=11,height=10)
    }
  }
}


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
