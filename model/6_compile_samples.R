library(rstan)

args <- commandArgs(trailingOnly = F)

print(args)

i <- args[(length(args)-1):length(args)]
frame <- strsplit(i[1],"--")[[1]][2]
source <- strsplit(i[2],"--")[[1]][2]

regexstr = paste0(frame,"_",source,".*.csv")

#get relevant csv files
csvfiles=dir(path='~/cmdstan/model/',pattern=regexstr)

print(csvfiles)

#create fit object from those csv files
fit=read_stan_csv(paste0('~/cmdstan/model/',csvfiles))

#save fit object
save(fit,file=paste0("~/cmdstan/model/",frame,"_",source,"_fit.RData"))
