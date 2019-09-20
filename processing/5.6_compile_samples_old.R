library(rstan)

args <- commandArgs(trailingOnly = T)

print(args)

i <- args[1]

run <- strsplit(i[1],"--")[[1]][2]

if(run == 1){cond = "ap"; source = "obs"}
if(run == 2){cond = "av"; source = "obs"}
if(run == 3){cond = "ap"; source = "opt"}
if(run == 4){cond = "av"; source = "opt"}

regexstr = paste0("expt123_",source,"_",cond,".*.csv")

#path_to_model = "cmdstan/model/"
path_to_model = "/QRISdata/Q0993/models/"

#get relevant csv files
csvfiles=dir(path=path_to_model,pattern=regexstr)

print(csvfiles)

#create fit object from those csv files
fit=read_stan_csv(paste0(path_to_model,csvfiles))

#save fit object
save(fit,file=paste0("/QRISdata/Q0993/data/derived/expt123_",source,"_",cond,"_fit.RData"))
