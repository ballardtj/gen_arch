rm(list=ls())

#load packages
library(rstan)
library(tidyverse)
library(tidybayes)

get_posts= function(fit){
  posts = tidy_draws(fit) %>%
    gather_variables() %>%
    mutate(subject = gsub(".*\\.","",as.character(.variable)),
           parameter = gsub("\\..*","",as.character(.variable))) %>%
    filter(parameter %in% c('w1','w2','w3','delta','tau','alpha')) %>%
    ungroup() %>%
    select(subject,.draw,parameter,.value) %>%
    spread(key=parameter,value=.value) %>%
    mutate(delta = if_else( w1 >= 0, delta, -delta),
           tau = -tau,
           w1 = abs(w1),
           w2 = abs(w2)) %>%
    gather(key=parameter,value=estimate,alpha:w3) %>%
    mutate(parameter = factor(parameter,levels=c('w1','w2','w3','delta','tau','alpha'))) %>%
    filter(!is.na(estimate))
  return(posts)
}


#########################
### Approach Observed ###
#########################

#Load fit objects
load("data/derived/ap_obs_fit.RData")
posts_ap_obs = get_posts(fit)
posts_ap_obs$frame = 'approach'
posts_ap_obs$source = 'observed'

load("data/derived/av_obs_fit.RData")
posts_av_obs = get_posts(fit)
posts_av_obs$frame = 'avoidance'
posts_av_obs$source = 'observed'

load("data/derived/ap_opt_fit.RData")
posts_ap_opt = get_posts(fit)
posts_ap_opt$frame = 'approach'
posts_ap_opt$source = 'optimal'

load("data/derived/av_opt_fit.RData")
posts_av_opt = get_posts(fit)
posts_av_opt$frame = 'avoidance'
posts_av_opt$source = 'optimal'

posts = bind_rows(posts_ap_obs,
                  posts_av_obs,
                  posts_ap_opt,
                  posts_av_opt)

pd1 = posts %>%
  group_by(.draw,parameter,frame,source) %>%
  summarise(estimate = mean(estimate))

#Mean parameters across conditions
ggplot(data=pd1) +
  geom_density(aes(x=estimate,fill=frame),alpha=0.5) +
  facet_grid(parameter~source,scale="free")

#CI of individual participants
pd2=posts %>%
  filter(source=="observed") %>%
  group_by(parameter,subject,frame) %>%
  point_interval(estimate) %>%
  group_by(parameter,frame) %>%
  arrange(estimate) %>%
  mutate(order = 1:n())

ggplot(data=pd2) +
  geom_pointintervalh(aes(y=order,x=estimate),size=0.1,alpha=0.5) +
  facet_grid(frame~parameter,scale="free")


#Mean of approach/avoidance observed optimal




#Mean across experiment


data.frame(
  x = rep(1:100,10),
  y = rnorm(1000, mean = 2, sd = 2)
) %>%
  group_by(x) %>%
  point_interval(y)

  geom_intervalh()

ggplot(posts) +
  geom_eyeh(aes(x = estimate, y = subject)) +
  facet_wrap(~parameter)

fit %>%
  spread_samples(w2[condition])

posts <- ggs(fit) %>%
  mutate(Subject =
          Family = gsub("\\..*","",as.character(posts$Parameter)))

strsplit(x=as.character(posts$Parameter[1:100]),split="\\.")[[]]






posts = bind_rows(data.frame(rstan::extract(fit,pars=c('w1',"w2","w3","delta","tau","alpha"))))


w1_posts <- ggs(fit,family="w1")
w2_posts <- ggs(fit,family="w2")
w3_posts <- ggs(fit,family="w3")
delta_posts <- ggs(fit,family="delta")
tau_posts <- ggs(fit,family="tau")
alpha_posts <- ggs(fit,family="alpha")

w1_plot = ggs_caterpillar(w1_posts) + labels(x="w1",y="subject")
w2_plot = ggs_caterpillar(w2_posts) + labels(x="w2",y="subject")
w3_plot = ggs_caterpillar(w3_posts) + labels(x="w3",y="subject")
delta_plot = ggs_caterpillar(delta_posts) + labels(x="delta",y="subject")
tau_plot = ggs_caterpillar(tau_posts) + labels(x="tau",y="subject")
alpha_plot = ggs_caterpillar(alpha_posts) + labels(x="alpha",y="subject")

posts <- ggs(fit)
ggs_caterpillar(posts,family="w1")

#Create fit objects
for(source in c('opt')){
  for(frame in c('ap','av')){
    #create string with regular expression
    regexstr = paste0(frame,"_",source,".*_v21_pl.csv")
    #get relevant csv files
    csvfiles=dir(path='./model_output/',pattern=regexstr)
    #create fit object from those csv files
    fit=read_stan_csv(paste0('./model_output/',csvfiles))
    #save fit object
    save(fit,file=paste0("./model_output/goal_",source,"_",frame,"_fit.RData"))
  }
}

#create vector of hyperparameter names
hypers=c('w1_mean','w1_sd','w2_mean','w2_sd','w3_mean','w3_sd',
         'delta_a','delta_b','tau_a','tau_b','alpha_a','alpha_b')

hypers=c('w1_mean','w1_sd','w2_mean','w2_sd','w3_mean','w3_sd',
         'delta_mean','delta_sd','tau_mean','tau_sd','alpha_mean','alpha_sd')

hypers=c('w1[1]','w1[2]','w2[1]','w2[2]','w3[1]','w3[2]',
         'delta[1]','delta[2]','tau[1]','tau[2]','alpha[1]','alpha[2]')

#########################
### Approach Observed ###
#########################

#Load fit objects
load("./model_output/goal_obs_ap_fit.RData")

smrystan = summary(fit)
smrystan[[1]][hypers,]

# mean      se_mean         sd       2.5%        25%
#   w1_mean    -4.2807135 4.553174e-03 0.35135446 -4.9865110 -4.5134800
# w1_sd       3.5557956 2.581125e-03 0.25780544  3.0788678  3.3780050
# w2_mean    -3.6049459 3.169626e-03 0.29216031 -4.1689415 -3.8017525
# w2_sd       6.4834903 3.184105e-03 0.43006776  5.6757897  6.1879700
# w3_mean     0.5717556 1.788446e-03 0.11124157  0.3418069  0.5003122
# w3_sd       2.9862159 1.999211e-03 0.17502498  2.6570385  2.8662750
# delta_mean  0.6555514 2.097784e-04 0.03052397  0.5949294  0.6350592
# delta_sd    0.2774716 1.573334e-04 0.02178852  0.2375200  0.2622220
# tau_mean    0.3915824 1.258909e-04 0.02758132  0.3381557  0.3728798
# tau_sd      0.2630919 9.337551e-05 0.02045755  0.2259550  0.2488407
# alpha_mean  0.5847639 1.083530e-04 0.02373896  0.5381019  0.5688075
# alpha_sd    0.3568964 7.649289e-05 0.01675875  0.3257249  0.3452730
# 50%        75%      97.5%     n_eff      Rhat
# w1_mean    -4.2778150 -4.0422750 -3.6092850  5954.735 1.0008181
# w1_sd       3.5461750  3.7210750  4.0880218  9976.220 1.0014516
# w2_mean    -3.6092000 -3.4113800 -3.0192740  8496.234 1.0007612
# w2_sd       6.4710750  6.7666375  7.3634210 18243.111 1.0000904
# w3_mean     0.5757630  0.6471705  0.7801678  3868.856 1.0026901
# w3_sd       2.9810700  3.0994100  3.3436415  7664.480 1.0015461
# delta_mean  0.6559900  0.6763903  0.7142431 21171.927 1.0003941
# delta_sd    0.2766265  0.2915190  0.3232332 19178.470 1.0004179
# tau_mean    0.3915105  0.4099555  0.4465402 48000.000 0.9999846
# tau_sd      0.2622285  0.2762985  0.3057220 48000.000 1.0003863
# alpha_mean  0.5849470  0.6008102  0.6311990 48000.000 1.0014170
# alpha_sd    0.3562955  0.3679235  0.3914411 48000.000 0.9999801



pdf("./model_output/ap_obs_traceplot.pdf",width=12,height=10,paper='special')
  rstan::traceplot(fit,par=hypers)
dev.off()

pdf("./model_output/ap_obs_pairsplot.pdf",width=20,height=20,paper='special')
  pairs(fit,pars=hypers)
dev.off()


##########################
### Avoidance Observed ###
##########################

#Load fit objects
load("./model_output/goal_obs_av_fit.RData")

smrystan = summary(fit)
smrystan[[1]][hypers,]

launch_shinystan(fit)

#Av obs
# mean      se_mean         sd          2.5%         25%
#   w1_mean    -6.03412322 1.609540e-02 0.46694681 -7.0048760000 -6.33772250
# w1_sd       3.55490979 6.303247e-03 0.28832694  3.0178992500  3.35760750
# w2_mean    -0.07676850 1.851046e-03 0.11440357 -0.3379178750 -0.09546858
# w2_sd       1.69532992 1.702484e-02 0.58753257  0.6856468500  1.28924500
# w3_mean     0.02340785 9.970704e-05 0.02184472  0.0006795281  0.00725615
# w3_sd       1.72902121 1.015490e-03 0.09071092  1.5593097500  1.66775750
# delta_mean  0.26912149 5.865237e-04 0.02860670  0.2158687750  0.24925550
# delta_sd    0.21811649 5.778930e-04 0.02400452  0.1708670000  0.20206600
# tau_mean    0.04657756 1.383723e-03 0.02722893  0.0206924250  0.03278055
# tau_sd      0.01658909 4.183881e-04 0.00933730  0.0047150655  0.01094578
# alpha_mean  0.32586698 2.127273e-03 0.01941319  0.2895995250  0.31238275
# alpha_sd    0.21690804 1.872903e-03 0.01374451  0.1918319000  0.20736775
# 50%         75%        97.5%       n_eff     Rhat
# w1_mean    -6.02061500 -5.71885500 -5.150200000   841.64994 1.011926
# w1_sd       3.54389000  3.74549500  4.147580000  2092.38618 1.008113
# w2_mean    -0.04376300 -0.01761288 -0.001556002  3819.83390 1.001891
# w2_sd       1.64650500  2.05600750  2.975958750  1190.96110 1.005536
# w3_mean     0.01701305  0.03291008  0.081713808 48000.00000 1.000575
# w3_sd       1.72574000  1.78657000  1.916620250  7979.35618 1.000551
# delta_mean  0.26856600  0.28801200  0.326913150  2378.83551 1.004345
# delta_sd    0.21803100  0.23409750  0.265840350  1725.40520 1.003861
# tau_mean    0.04231875  0.05438088  0.091628570   387.22468 1.020149
# tau_sd      0.01515545  0.02042392  0.035277438   498.06265 1.016523
# alpha_mean  0.32535100  0.33876400  0.365340250    83.28135 1.032258
# alpha_sd    0.21623100  0.22570150  0.245863150    53.85526 1.038526

pdf("./model_output/av_obs_traceplot.pdf",width=12,height=10,paper='special')
  rstan::traceplot(fit,par=hypers)
dev.off()

pdf("./model_output/av_obs_pairsplot.pdf",width=20,height=20,paper='special')
  pairs(fit,pars=hypers)
dev.off()

########################
### Approach Optimal ###
########################

#Load fit objects
load("./model_output/goal_opt_ap_fit.RData")

smrystan = summary(fit)
smrystan[[1]][hypers,]

#Ap opt
# mean     se_mean         sd         2.5%         25%         50%
# w1_mean -55.0829992 0.025133362 2.55911888 -60.22590000 -56.7787250 -55.0435500
# w1_sd     0.7852351 0.002699193 0.59136357   0.03051026   0.3150763   0.6657450
# w2_mean -29.2527742 0.042760304 0.87495695 -31.01284000 -29.8216000 -29.2370000
# w2_sd     4.1071607 0.024184848 0.88587780   2.24066975   3.5600850   4.1384850
# w3_mean  10.6111074 0.014062086 0.32642397   9.94674675  10.3980000  10.6164000
# w3_sd    10.9004447 0.003335237 0.41791933  10.10139500  10.6135750  10.8943000
# delta_a   1.9792451 0.001610873 0.22909191   1.55492950   1.8201475   1.9709550
# delta_b  29.1195309 0.018349393 3.23626268  22.99409500  26.8976750  29.0541500
# tau_a     2.1968091 0.010452720 0.73188138   1.16763925   1.6923900   2.0685750
# tau_b     0.1863753 0.001060252 0.05569804   0.11025092   0.1479737   0.1756915
# alpha_a  32.1094907 0.798404057 4.67279664  14.29185750  30.8500750  32.6353000
# alpha_b  11.3014540 0.267413694 1.58446248   5.37516400  10.8463250  11.4766500
# 75%       97.5%       n_eff      Rhat
# w1_mean -53.3324750 -50.1845975 10367.63654 1.0010500
# w1_sd     1.1348550   2.2007603 48000.00000 0.9999678
# w2_mean -28.6720000 -27.5423000   418.68925 1.0166983
# w2_sd     4.6998125   5.7591793  1341.71696 1.0084745
# w3_mean  10.8323000  11.2336025   538.84594 1.0131525
# w3_sd    11.1788250  11.7385050 15701.15483 1.0022795
# delta_a   2.1277025   2.4495933 20225.38308 1.0007335
# delta_b  31.2631000  35.6839000 31105.99536 1.0001787
# tau_a     2.5517975   3.9765668  4902.55821 1.0010945
# tau_b     0.2128198   0.3241944  2759.69970 1.0023487
# alpha_a  34.4233500  37.9001100    34.25376 1.2112161
# alpha_b  12.1077250  13.3085050    35.10724 1.2045677


pdf("./model_output/ap_opt_traceplot.pdf",width=12,height=10,paper='special')
rstan::traceplot(fit,par=hypers)
dev.off()

pdf("./model_output/ap_opt_pairsplot.pdf",width=20,height=20,paper='special')
pairs(fit,pars=hypers)
dev.off()


#########################
### Avoidance Optimal ###
#########################

#Load fit objects
load("./model_output/goal_opt_av_fit.RData")

smrystan = summary(fit)
smrystan[[1]][hypers,]

#Av opt
# mean     se_mean         sd         2.5%         25%         50%
# w1_mean -43.6195703 0.003066720 0.67188464 -44.95120500 -44.0691000 -43.6132000
# w1_sd     2.2770470 0.017507974 1.00943431   0.20564990   1.5976625   2.3614500
# w2_mean -27.9736196 0.035245907 2.37796964 -32.87781500 -29.5102000 -27.8870500
# w2_sd     1.2363518 0.004375523 0.77405729   0.07724138   0.6275162   1.1463400
# w3_mean   4.3198814 0.002149926 0.18340354   3.94497800   4.1994475   4.3246500
# w3_sd    12.1329255 0.005753716 0.40010322  11.36559750  11.8578000  12.1277000
# delta_a   6.7578705 0.004831609 0.90375037   5.11241025   6.1267575   6.7157400
# delta_b  13.2273687 0.009645552 1.80603427   9.94864200  11.9617000  13.1370000
# tau_a     0.4904137 0.000523784 0.06876304   0.36604678   0.4423830   0.4865975
# tau_b    12.4225518 0.016912104 2.24859466   8.35501650  10.8342000  12.3118000
# alpha_a   3.7417134 0.160030826 0.45641711   2.70059725   3.4964450   3.7888400
# alpha_b   2.5567440 0.089893755 0.28048335   1.94172825   2.3907900   2.5747500
# 75%       97.5%        n_eff      Rhat
# w1_mean -43.1670000 -42.3187000 48000.000000 1.0003093
# w1_sd     3.0152125   4.0646020  3324.178385 1.0030024
# w2_mean -26.3047250 -23.5783900  4551.926371 1.0008193
# w2_sd     1.7412975   2.9586630 31295.818211 0.9999987
# w3_mean   4.4464400   4.6630915  7277.271585 1.0028364
# w3_sd    12.3979000  12.9383000  4835.565904 1.0028631
# delta_a   7.3396550   8.6445128 34987.544694 0.9999564
# delta_b  14.3933500  17.0243150 35058.857865 1.0000310
# tau_a     0.5353385   0.6330305 17234.771974 1.0003042
# tau_b    13.8706250  17.1297100 17677.752537 1.0001329
# alpha_a   4.0446100   4.5436002     8.134232 1.3796929
# alpha_b   2.7460475   3.0668730     9.735430 1.2851120

pdf("./model_output/av_opt_traceplot.pdf",width=12,height=10,paper='special')
  rstan::traceplot(fit,par=hypers)
dev.off()

pdf("./model_output/av_opt_pairsplot.pdf",width=20,height=20,paper='special')
  pairs(fit,pars=hypers)
dev.off()


#
# #Only appears converged after 2500 iterations. Recheck ESS and GD based on last 500 samples
#
# mcmc_approach_obs2=mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[1501:2000,x,])))
#
# smry2=summary(mcmc_approach_obs2)
# gelmans2=gelman.diag(mcmc_approach_obs2)
# ess2=effectiveSize(mcmc_approach_obs2)
#
# smry2[[1]][hypers,] #pretty much the same posteriors as rstan
#
# # Mean          SD     Naive SE Time-series SE
# # w1_mean    -1.632987516 0.447350560 4.083733e-03   1.102828e-02
# # w1_sd       4.199786089 0.349184947 3.187608e-03   5.711894e-03
# # w2_mean    -0.627173151 0.295784759 2.700133e-03   8.258312e-03
# # w2_sd       2.503409281 0.372789282 3.403085e-03   5.294717e-03
# # w3_mean     0.003658913 0.002860135 2.610934e-05   4.417003e-05
# # w3_sd      10.946473574 0.633409265 5.782209e-03   8.467630e-03
# # delta_mean  0.264140920 0.447709306 4.087008e-03   9.473926e-03
# # delta_sd    2.874595201 0.392954525 3.587168e-03   6.417644e-03
# # tau_mean   -3.916749292 0.588019771 5.367862e-03   1.216023e-02
# # tau_sd      4.836670483 0.488561612 4.459937e-03   5.210324e-03
# # alpha_mean -1.650154719 0.216092407 1.972645e-03   8.317806e-03
# # alpha_sd    2.737521637 0.177789456 1.622988e-03   4.175149e-03
#
# gelmans2[[1]][hypers,]
#
# # Point est. Upper C.I.
# # w1_mean      1.074281   1.121376
# # w1_sd        1.026752   1.045028
# # w2_mean      1.027135   1.045674
# # w2_sd        1.005433   1.010123
# # w3_mean      1.033549   1.051768
# # w3_sd        1.025112   1.042472
# # delta_mean   1.068892   1.113359
# # delta_sd     1.014394   1.024869
# # tau_mean     1.015297   1.026240
# # tau_sd       1.000444   1.001785
# # alpha_mean   1.070671   1.116346
# # alpha_sd     1.094825   1.153426
#
# ess2[hypers]
# # w1_mean      w1_sd    w2_mean      w2_sd    w3_mean      w3_sd
# # 1749.0027  3815.6408  1613.0534  5615.3819 12915.4075  5902.6854
# # delta_mean   delta_sd   tau_mean     tau_sd alpha_mean   alpha_sd
# # 2323.8968  4065.5986  2681.0519  9912.2041   883.6365  2067.7927
#
#
#
#
# ##########################
# ### Avoidance Observed ###
# ##########################
#
# rm(list=ls())
#
# #load packages
# library(rstan)
# library(coda)
#
# #Load fit objects
# load("./model_output/goal_obs_avoidance_fit.RData")
#
# #rstan
# hypers=c('w1_mean','w1_sd','w2_mean','w2_sd','w3_mean','w3_sd',
#          'delta_mean','delta_sd','tau_mean','tau_sd','alpha_mean','alpha_sd')
#
# smrystan = rstan::summary(fit)
# smrystan[[1]][hypers,]
#
# # mean      se_mean          sd          2.5%          25%
# #   w1_mean     1.171730221 0.0170406113 0.302462928  0.5635798810  0.973560341
# # w1_sd       2.915538502 0.0299500760 0.282474973  2.4175107966  2.715825410
# # w2_mean    -0.903452785 0.0050390034 0.289835398 -1.3834050348 -1.105003629
# # w2_sd       1.826335517 0.0031410522 0.313123944  1.2817902056  1.604698354
# # w3_mean     0.008002709 0.0002381943 0.006400688  0.0003012864  0.003159912
# # w3_sd       9.715691567 0.0192914413 0.678392402  8.3952273893  9.255154325
# # delta_mean  1.111082368 0.0180263323 0.446059876  0.2771110448  0.808147954
# # delta_sd    2.633728235 0.0030033057 0.370187572  1.9814521672  2.374294270
# # tau_mean   -4.051276743 0.0096253334 0.602742070 -5.2470376740 -4.447127471
# # tau_sd      5.030328590 0.0022197333 0.486319190  4.1387498963  4.690469251
# # alpha_mean -2.364467740 0.0104951672 0.207276156 -2.7811598272 -2.501373479
# # alpha_sd    2.436177124 0.0158700917 0.187117692  2.1001952283  2.304194248
# # 50%         75%       97.5%       n_eff     Rhat
# # w1_mean     1.175132156  1.37312849  1.75915642   315.04600 1.041552
# # w1_sd       2.895765087  3.09250861  3.52228166    88.95372 1.131479
# # w2_mean    -0.937641187 -0.73866246 -0.20645810  3308.36598 1.006557
# # w2_sd       1.802085916  2.02212465  2.50065053  9937.61668 1.001939
# # w3_mean     0.006598593  0.01126744  0.02409944   722.08897 1.022952
# # w3_sd       9.711775436 10.17259137 11.05693299  1236.60956 1.015079
# # delta_mean  1.093564098  1.40045450  2.03416590   612.31031 1.021427
# # delta_sd    2.609999074  2.86870600  3.42659578 15193.03625 1.002733
# # tau_mean   -4.046902337 -3.65149872 -2.87549565  3921.31241 1.005840
# # tau_sd      5.010051977  5.34731301  6.04505578 48000.00000 1.001798
# # alpha_mean -2.362587925 -2.22131966 -1.97315017   390.04972 1.048451
# # alpha_sd    2.425460444  2.55678563  2.83338763   139.01793 1.140992
#
# mcmc_avoidance_obs=mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
#
# smry=summary(mcmc_avoidance_obs)
# gelmans=gelman.diag(mcmc_avoidance_obs)
# ess=effectiveSize(mcmc_avoidance_obs)
#
# smry[[1]][hypers,] #same posteriors as rstan
# gelmans[[1]][hypers,]
#
# # Point est. Upper C.I.
# # w1_mean      1.049653   1.080921
# # w1_sd        1.137206   1.218135
# # w2_mean      1.007831   1.013044
# # w2_sd        1.001581   1.002898
# # w3_mean      1.023250   1.038000
# # w3_sd        1.016064   1.026756
# # delta_mean   1.029969   1.049402
# # delta_sd     1.002251   1.003992
# # tau_mean     1.006475   1.010937
# # tau_sd       1.002309   1.004101
# # alpha_mean   1.039786   1.065168
# # alpha_sd     1.146502   1.232242
# # Multivariate psrf
# #
# # 5.84
#
# ess[hypers] #way higher
# # w1_mean      w1_sd    w2_mean      w2_sd    w3_mean      w3_sd
# # 7219.741   5681.517   4583.117  13426.475  17285.342  14129.406
# # delta_mean   delta_sd   tau_mean     tau_sd alpha_mean   alpha_sd
# # 11096.639  17883.766   9224.201  40097.179   3308.880   3699.192
#
# pdf("./model_output/avoidance_obs_traceplot_2000.pdf",width=20,height=20,paper='special')
#   rstan::traceplot(fit,par=hypers,inc_warmup=F)
# dev.off()
#
# pdf("./model_output/avoidance_obs_traceplot_500.pdf",width=20,height=20,paper='special')
#   rstan::traceplot(fit,par=hypers,inc_warmup=F,window=c(2501,3000))
# dev.off()
#
#
#
#
#
#
#
#
#
#
# #
# # min=1001
# # max=dim(fit)[1]
# #
# # mcmc2=mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[min:max,x,])))
# #
# # summary(mcmc)
# #
# # summary(mcmc2)
# #
# # gd1=gelman.diag(mcmc)
# #
# # gd2=gelman.diag(mcmc2)
# #
# # coda::traceplot(mcmc, col = "w1_mean")
#
#
#
# #Get parameters
#
# w1_mean = extract(fit,par="w1_mean")[[1]]
# w1_sd = extract(fit,par="w1_sd")[[1]]
# w2_mean = extract(fit,par="w2_mean")[[1]]
# w2_sd = extract(fit,par="w2_sd")[[1]]
# w3_mean = extract(fit,par="w3_mean")[[1]]
# w3_sd = extract(fit,par="w3_sd")[[1]]
#
# delta_mean = extract(fit,par="delta_mean")[[1]]
# delta_sd = extract(fit,par="delta_sd")[[1]]
# tau_mean = extract(fit,par="tau_mean")[[1]]
# tau_sd = extract(fit,par="tau_sd")[[1]]
# alpha_mean = extract(fit,par="alpha_mean")[[1]]
# alpha_sd = extract(fit,par="alpha_sd")[[1]]
#
# #Spatial gradient
# w1 = extract(fit,par="w1")[[1]]
# delta = extract(fit,par="delta")[[1]]
# distance = seq(0,1,by=0.01)
# spatial_gradient_mean = array(NA,dim=c(ncol(w1),length(distance)))
#
# for(i in 1:ncol(w1)){
#   w1_sub_tmp = w1_mean + w1[,i]*w1_sd
#   w1_sub_tmp2 = abs(w1_sub_tmp)
#   delta_sub_tmp = 1/(1+exp(-(delta_mean + delta[,i]*delta_sd)))
#   delta_sub_tmp2 = (w1_sub_tmp>=0)*delta_sub_tmp + (w1_sub_tmp<0)*-delta_sub_tmp
#   for( j in 1:length(distance)){
#     spatial_gradient_mean[i,j] = mean((delta_sub_tmp2<0)*w1_sub_tmp2*(1-distance[j]^-delta_sub_tmp2) +
#                                  (delta_sub_tmp2>=0)*w1_sub_tmp2*(1-distance[j]^delta_sub_tmp2))
#   }
#   print(i)
# }
#
# library(reshape2)
# spatial_gradient=melt(spatial_gradient_mean)
# names(spatial_gradient) = c('subject','distance','value')
#
# ggplot(data=spatial_gradient) +
#   geom_line(aes(x=distance,y=value,group=subject,colour=subject))
#
# #Temporal gradient
# w2 = extract(fit,par="w2")[[1]]
# tau = extract(fit,par="tau")[[1]]
# time = seq(0,1,by=0.01)
# temporal_gradient_mean = array(NA,dim=c(ncol(w2),length(time)))
#
# for(i in 1:ncol(w2)){
#   w2_sub_tmp = w2_mean + w2[,i]*w2_sd
#   w2_sub_tmp2 = abs(w2_sub_tmp)
#   tau_sub_tmp = 1/(1+exp(-(tau_mean + tau[,i]*tau_sd)))
#   tau_sub_tmp2 = (w2_sub_tmp>=0)*tau_sub_tmp + (w2_sub_tmp<0)*-tau_sub_tmp
#   for( j in 1:length(distance)){
#     temporal_gradient_mean[i,j] = mean((tau_sub_tmp2<0)*w2_sub_tmp2*(1-time[j]^-tau_sub_tmp2) +
#                                         (tau_sub_tmp2>=0)*w2_sub_tmp2*(1-time[j]^tau_sub_tmp2))
#   }
#   print(i)
# }
#
# # w3=rnorm(n=10000,
# #          mean=rnorm(n=10000,mean(w3_mean),sd(w3_mean)),
# #          sd=rnorm(n=10000,mean(w3_sd),sd(w3_sd)))
#
# library(reshape2)
# temporal_gradient=melt(temporal_gradient_mean)
# names(temporal_gradient) = c('subject','time','value')
#
# ggplot(data=temporal_gradient) +
#   geom_line(aes(x=time,y=value,group=subject,colour=subject))
#
#
# #Spatiotemporal gradient
# w3 = extract(fit,par="w3")[[1]]
# alpha = extract(fit,par="alpha")[[1]]
# spacetime =  seq(0.1,10,by=0.1)
# spatiotemporal_gradient_mean = array(NA,dim=c(ncol(w3),length(spacetime)))
#
# for(i in 1:ncol(w3)){
#   w3_sub_tmp = w3_mean + w3[,i]*w3_sd
#   alpha_sub_tmp = 1/(1+exp(-(alpha_mean + alpha[,i]*alpha_sd)))
#   sg_max = 2*(1-alpha_sub_tmp)*sqrt(alpha_sub_tmp/(1-alpha_sub_tmp))
#
#   for( j in 1:length(spacetime)){
#     spatiotemporal_gradient_mean[i,j] = mean(w3_sub_tmp*sg_max / (alpha_sub_tmp /spacetime[j] + (1-alpha_sub_tmp )*spacetime[j]))
#   }
#   print(i)
# }
#
# library(reshape2)
# spatiotemporal_gradient=melt(spatiotemporal_gradient_mean)
# names(spatiotemporal_gradient) = c('subject','spacetime','value')
#
# ggplot(data=spatiotemporal_gradient) +
#   geom_line(aes(x=spacetime/10,y=value,group=subject,colour=subject)) +
#   coord_trans(x = "log10")
#
#
#
#
#
# spatial_gradient =
#
#
# w1_t =
#
#   x= t( w1_sd * t(w1))
#
