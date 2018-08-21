library(rstan)

args <- commandArgs(trailingOnly = T)

print(args)

i <- args[(length(args)-3):length(args)]
frame <- strsplit(i[1],"--")[[1]][2]
source <- strsplit(i[2],"--")[[1]][2]
structure <- strsplit(i[3],"--")[[1]][2]
model <- strsplit(i[4],"--")[[1]][2]

regexstr = paste0(frame,"_",source,".*",structure,"_",model,".*.csv")

#get relevant csv files
csvfiles=dir(path='~/cmdstan/model/',pattern=regexstr)

print(csvfiles)

#create fit object from those csv files
fit=read_stan_csv(paste0('~/cmdstan/model/',csvfiles))

#save fit object
save(fit,file=paste0("~/cmdstan/model/",frame,"_",source,"_",structure,"_",model,"_fit.RData"))
