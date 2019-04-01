### clear workspace ###

rm(ls=list())

### load packages ###

library(rstan)
library(coda)
library(knitr)

### Experiments 1, 2, and 3 ###

parms = c('w1_mean','w1_sd','w2_mean','w2_sd','w3_mean','w3_sd',
  'delta_mean','delta_sd','tau_mean','tau_sd','alpha_mean','alpha_sd')

# Approach

load("data/derived/expt123_ap_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))

# Avoidance

load("data/derived/expt123_av_fit.RData")

smrystan = summary(fit)
print(kable(round(smrystan[[1]][parms,],3),format="markdown"))






