library(rstan)

args <- commandArgs(trailingOnly = F)

print(args)

i <- args[(length(args)-1):length(args)]
i <- strsplit(i,"--")[[1]][2]

print(i)

getwd()

regexstr = paste0(i[1],"_",i[2],".*.csv")

#get relevant csv files
csvfiles=dir(path='~/cmdstan/model/',pattern=regexstr)
print(csvfiles)

#create fit object from those csv files
fit=read_stan_csv(paste0('~/cmdstan/model/',csvfiles))
      
#save fit object
save(fit,file=paste0("~/cmdstan/model/",i[1],"_",i[2],"_fit.RData"))
