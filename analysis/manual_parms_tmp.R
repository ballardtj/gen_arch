x = filter(posts,
  structure=='hier',
  model=='space',
  parameter %in% c('w1','w2'),
  source=='obs',
  frame=='ap')

x %>%
  filter(subject==1) %>%
  group_by(parameter) %>%
  point_interval(estimate)

#obs, av, fixed, nospace
samps = extract(fit)
mean_w1 = apply(abs(samps$w1),1,mean)
mean_w2 = apply(abs(samps$w2),1,mean)

quantile(mean_w1,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.7899896 1.0566071 1.3766211
quantile(mean_w2,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.7899896 1.0566071 1.3766211

#obs, ap, hier, space
source='obs'
frame='ap'
structure='hier'
model='nospace'
load(paste0("data/derived/",frame,"_",source,"_",structure,"_",model,"_fit.RData"))
samps = extract(fit)
mean_w1 = apply(abs(samps$w1),1,mean)
mean_w2 = apply(abs(samps$w2),1,mean)

quantile(mean_w1,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.7017712 0.7908023 0.8893249
quantile(mean_w2,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.8221922 0.9163819 1.0192931

#obs, av, hier, space
source='obs'
frame='av'
structure='hier'
model='nospace'
load(paste0("data/derived/",frame,"_",source,"_",structure,"_",model,"_fit.RData"))
samps = extract(fit)
mean_w1 = apply(abs(samps$w1),1,mean)
mean_w2 = apply(abs(samps$w2),1,mean)

quantile(mean_w1,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.6966252 0.7893899 0.8893780
quantile(mean_w2,c(0.025,0.5,0.975))
# 2.5%       50%     97.5%
# 0.6902861 0.7952417 0.9079748

#priors

pr = abs(rnorm(n=10000,abs(rnorm(n=10000,mean=0,sd=5)),abs(rnorm(n=10000,mean=0,sd=1))))

#TODO! - Rerun model with optimal decisions calculated properly (already updated the script to calculate decisions, just need to relaunch models)
#TODO! - change models so that priors are on the real parameter, not on the transform of it
