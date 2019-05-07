library(rstan)

args <- commandArgs(trailingOnly = T)

print(args)

i <- args[1]

run <- strsplit(i[1],"--")[[1]][2]

if(run == 1){cond = "ap"}
if(run == 2){cond = "av"}

regexstr = paste0("expt123_",cond,".*.csv")

#path_to_model = "cmdstan/model/"
path_to_model = "/QRISdata/Q0993/cmdstan/model/"

#get relevant csv files
csvfiles=dir(path=path_to_model,pattern=regexstr)

print(csvfiles)

#create fit object from those csv files
fit=read_stan_csv(paste0(path_to_model,csvfiles))

#save fit object
save(fit,file=paste0(path_to_model,"expt123_",cond,"_fit.RData"))
