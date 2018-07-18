#start new R session
.rs.restartR()

#run bash script to compile model
system2(command="bash",args="model/make_goal.sh")
system2(command="bash",args="model/upload_to_cluster.sh")

#run second bash script to upload files to cluster
system2(command="scp",args=c("-r","model","uqtballa@tinaroo.rcc.uq.edu.au:~/"))
system2(command="scp",args=c("data/clean/obs_approach_rdump.R","uqtballa@tinaroo.rcc.uq.edu.au:~/model"))



#start new R session
.rs.restartR()

#run bash script to compile model
system2(command="bash",args="model/make_goal.sh")

#run second bash script to upload files to cluster
system2(command="scp",args=c("-r","model/cmdstan","uqtballa@tinaroo.rcc.uq.edu.au:~/"))



system2(command="ssh",args=c("uqtballa@tinaroo.rcc.uq.edu.au","mkdir goal_tmp"))
system2(command="scp",args=c("model/goal","uqtballa@tinaroo.rcc.uq.edu.au:~/goal_tmp"))



system("make models/cmdstan/goal")

system2(command="make",args=c("models/cmdstan/goal","makdir goal_tmp"))

##read in the arguments listed at the command line

# args <- commandArgs(trailingOnly = F)
#
# print(args)
#
# i <- args[length(args)]
# i <- strsplit(i,"--")[[1]][2]
# i <- as.numeric(i)
# print(i)

models = c('obs_approach','obs_avoidance','opt_approach','opt_avoidance')
i=1
#load packages
library(rstan)

# #set working directory
# setwd("~/Stan/GenArch/Multinode/11-faster")

#load data
dataList=read_rdump('./clean_data/obs_approach_rdump.R')

fit1 <- stan(file="./analysis/models/LogModelVectorisedBounded_v22.stan",
            data = dataList,
            warmup = 0,
            iter = 200,
            chains = 1,
            thin = 1,
            cores = 1,
            seed=123123,
            control=list(adapt_delta=0.8)) #target acceptance probability

# Elapsed Time: 5e-06 seconds (Warm-up)
# 11.5991 seconds (Sampling)
# 11.5991 seconds (Total)

fit2 <- stan(file="./analysis/models/LogModelVectorisedBounded_v18.stan",
             data = dataList,
             warmup = 0,
             iter = 200,
             chains = 1,
             thin = 1,
             cores = 1,
             seed=123123,
             control=list(adapt_delta=0.8)) #target acceptance probability

# Elapsed Time: 2e-06 seconds (Warm-up)
# 9.61574 seconds (Sampling)
# 9.61574 seconds (Total)

fit2 <- stan(file="./analysis/models/LogModelVectorisedBounded_v19.stan",
             data = dataList,
             warmup = 0,
             iter = 200,
             chains = 1,
             thin = 1,
             cores = 1,
             seed=123123,
             control=list(adapt_delta=0.8)) #target acceptance probability

# Elapsed Time: 4e-06 seconds (Warm-up)
# 10.2336 seconds (Sampling)
# 10.2336 seconds (Total)

  parms = c("w1_mean","w1_sd","w2_mean","w2_sd","w3_mean","w3_sd","delta_mean","delta_sd","tau_mean","tau_sd","alpha_mean","alpha_sd","lp__")

  smry=summary(fit)
  smry$summary[parms,]

 save(fit,file=paste0("./model_output/",models[i],"_fit_first3_newpriors.RData"))

pdf(paste0("./model_output/",models[i],"_traceplot_first3_newpriors.pdf"),width=20,height=20,paper='special')
  traceplot(fit,par=parms,inc_warmup=T)
dev.off()

  pdf(paste0("./model_output/",models[i],"_pairs_first3_newpriors.pdf"),width=20,height=20,paper='special')
    pairs(fit,pars=parms)
  dev.off()

  #save(fit, file = paste0("/30days/uqtballa/goal_",models[i],"_fit.RData"))



 #New priors




