

library(rstan)
library(tidyverse)

#load stan function for generating predictions
expose_stan_functions("cmdstan/model/goal_hier_space_expt123.stan")
goal_sub_hier_space = goal_sub



dataList=read_rdump(paste0('data/clean/obs_av_rdump_expt123.R'))

hypers = c(1.82465,0.159182,-0.238309,1.29129,0.286073,0.591637)
w1 = c(1.98764,0.148193,-0.778897,-1.76878,-1.93241,0.900866,1.27243,0.810968,-0.272476,0.439653,-1.50587,1.6352,-1.79665,0.70572,1.01047,-0.627744,1.75257,-0.240998,1.56842,0.286376,1.96424,-0.43821,-1.45063,-1.73786,-0.0180265,-0.463381,0.824047,-1.6493,-1.36541,1.40147,-0.0489131,1.43017,0.800419,-0.107161,0.812512,1.67943,-1.38861,-1.22233,1.24082,-0.791482,0.449878,-1.66485,-1.78543,0.82771,-1.33126,-1.23532,0.0460619,0.769474,-0.653774,1.82167,0.558506,-0.884311,-1.05326,0.643211,-0.495399,0.772352,-1.77233,0.316604,1.99403,-1.91287,1.51475,1.72678,-0.670985,-1.8055,1.75428,0.0820209,-0.961602,0.29121,0.13239,1.51908,0.448239,-0.65849,1.57428,-1.85257,1.39709,1.11796,1.16722,-1.97689,1.52281,-0.0436506,0.211293,-1.4698,-1.87878,-1.12644,-0.327803,0.787556,0.102517,0.336023,1.49247,0.468982,0.634317,-0.942494,-0.523584,-1.1997,-1.20872,0.130486,-1.54655,1.83224,0.753529,0.851999,0.869758,1.91967,0.607082,0.961441,1.58913,0.453257,-1.94488,-0.216803,-0.843988,-1.64375,0.959663,-0.867265,-0.880582,1.07028,1.2188,1.1981,-1.67641,0.480015,-1.18465,-1.78082,-1.22886,-1.10753,0.47472,1.53767,1.56102,0.149773,-1.937,-0.725597,-0.838269,1.68351,1.55069,-1.83745,0.710546,-0.197749,-0.0553418,-1.115,-0.0324676,-1.03061,-1.91129,0.627587,1.8609,-1.06162,-1.76864,-1.08371,0.855906,-0.971616,0.811677,1.08522,1.11963,-0.409271,-1.58734,0.635889,0.598795,1.16438,1.88002,1.79327,-0.0121739,1.49616,-0.735806,-1.8677,0.868488,1.39187,0.18634,-1.21758,0.899251,-0.138816,1.3416,-1.9637,1.65121,-0.11124,1.65784,-0.499518,-0.694869,-1.39528,1.04745,1.77342,-0.0901128,0.549394,-0.725888,-1.11506)
w2 = c(-2.15424,-0.593557,-5.15618,-0.225692,-6.35494,-0.243124,-1.94122,-0.164299,-0.261855,-1.66337,-0.929124,-1.5096,-4.848,-0.276242,-4.21175,-0.336663,-0.841843,-0.431956,-2.2013,-0.9618,-2.132,-7.10205,-1.47761,-0.444226,-5.34009,-2.80634,-0.419311,-6.75398,-1.19931,-0.232982,-2.08106,-0.39308,-0.328581,-0.740456,-2.24202,-0.647803,-4.02945,-0.238841,-0.201548,-0.217369,-2.00811,-0.224972,-0.145738,-0.768062,-1.15029,-0.137692,-0.385766,-0.191472,-0.201511,-0.38881,-2.21673,-0.221105,-0.143481,-0.314909,-2.89824,-0.14175,-1.69389,-0.845156,-5.94396,-7.22898,-1.573,-0.147671,-0.274093,-1.29916,-4.56147,-0.568344,-0.447376,-2.39282,-4.55743,-5.70572,-0.292317,-0.785214,-1.78246,-0.761903,-1.52058,-2.89287,-1.5511,-0.328368,-0.982071,-0.378306,-0.210081,-0.223444,-0.201454,-1.14568,-5.12455,-1.06362,-0.161469,-1.12503,-0.175843,-5.19054,-1.3093,-0.184136,-0.603663,-1.34688,-0.851679,-0.892729,-2.77394,-0.370991,-0.422159,-2.51519,-0.583275,-0.164166,-1.12595,-0.321568,-0.516977,-0.744142,-0.147821,-0.401944,-1.58317,-6.66002,-0.959376,-1.37183,-0.336003,-0.728549,-0.868121,-0.30984,-0.181514,-0.495294,-1.35916,-0.38031,-4.70174,-1.3063,-1.62522,-0.406994,-6.97022,-0.705211,-5.93119,-4.32814,-0.355316,-1.36484,-3.62137,-0.262173,-1.67363,-0.344606,-0.286394,-1.92174,-0.838703,-0.886261,-0.21763,-0.187606,-0.683375,-3.2195,-0.239434,-0.742523,-6.04627,-3.42608,-0.365269,-0.23853,-1.54999,-0.692945,-2.84707,-0.318492,-6.90849,-2.35564,-1.11835,-5.8357,-0.141441,-6.71879,-2.57664,-4.32891,-0.503223,-3.78965,-0.321828,-4.92313)
w3 = c(5.4084,2.31363,1.27536,0.256322,0.260775,0.497251,0.793704,0.148385,3.94964,0.234176,1.67782,1.43073,3.22037,3.92351,0.234492,1.65963,0.25142,2.19306,0.257372,1.99522,0.344017,0.165867,2.15945,0.160166,0.71173,0.144879,0.993472,2.56905,0.31384,0.635142,0.87915,0.136199,3.54156,1.14026,0.887184,2.6602,0.49818,5.01266,0.186474,0.532766,1.00478,0.316888,1.44218,0.308304,0.282859,0.586836,0.653621,2.51116,0.331663,6.17455,0.387389,0.814716,4.48227,2.43813,0.448436,1.22079,0.275894,2.81597,2.05985,6.28831,0.67518,1.47905,6.48563,1.4775,1.35267,0.241201,6.82485,2.19508,2.09393,3.30419,6.20628,0.324174,0.280447,0.448043,0.156046,6.49374,0.657959,4.95885,0.8531,1.34619,5.45697,0.907087,0.536668,1.34253,4.52835,0.915931,0.856816,0.621078,5.66463,0.212717,5.22434,6.66413,0.192977,6.56324,0.596638,1.48226,0.408703,6.51407,1.17294,4.51088,0.309837,1.17863,0.951867,0.594917,0.333225,1.08198,3.24368,1.11335,1.39075,2.8987,5.33982,1.26554,0.817763,5.5525,0.251628,0.171481,2.98989,0.184302,4.46526,0.301368,0.288907,0.988633,0.297754,0.378207,2.64718,0.44397,0.181465,2.04661,0.330155,0.172686,0.211501,0.200027,0.544247,0.383561,3.72661,0.961394,0.615456,4.25263,1.59829,0.817477,1.31451,0.231834,1.83401,2.34843,3.64,0.266644,2.25459,3.2692,0.305143,4.05489,0.138951,0.141213,0.303512,6.09513,0.275306,0.312251,6.14001,0.697176,2.92209,0.480717,4.34605,0.691018,0.601885,0.440584,3.66015,3.32961,0.709485,0.614358,1.82137,6.62494,0.144569,0.44199,0.529649,1.12412,3.80881,6.13327,6.57925,0.719611,1.75141,0.668876,1.08656,0.913316,0.139458,2.80824,1.95567,1.32851,4.30997,1.19484,6.50583,6.61719,3.08005,0.387198,0.430209,6.96339,3.9156,0.463546,1.50034,0.503566,1.41631,4.99255,2.61176,0.378345,0.274638,0.236745,0.677309,0.37975,2.83889,1.90461,0.271415,1.92653,4.79588,2.16168,0.829442,0.154323,7.00439,3.47514,0.410817,0.245067,3.82555,2.55391,0.808266,0.50368,0.849346,3.19618,1.21486,1.40666,1.90041,4.96416,2.4555,0.436633,6.89927,0.136452,2.89067,0.711782,0.394837,0.154093,0.83412,4.24684,1.64619,0.189645,0.175217,0.572564,0.221018,0.446387,3.33128,3.85717,0.228993,0.345116,0.692462,3.68593,0.153834,3.91048,0.219513,0.266426,2.96159,5.17109,0.483378,4.78299,1.29895,0.302711,0.206788,5.22307,3.65098,5.49581,6.47649,6.66808,1.87651,0.360727,1.29992,4.47175,1.56111,1.32927,0.161313,7.36603,0.632167,4.46111,0.640367,1.24602,0.158288,0.163544,3.12333,2.79226,0.318912,0.465437,0.952353,1.6957,0.221918,0.239998,0.74142,0.516338,5.81595,5.81399,2.25091,0.856209)
delta = c(0.654755,0.358545,0.880197,0.773285,0.127981,0.224602,0.697211,0.600477,0.159714,0.590271,0.480362,0.532221,0.265878,0.379636,0.618118,0.619001,0.8327,0.649814,0.659849,0.80859,0.127579,0.239228,0.134115,0.533449,0.532621,0.246319,0.740005,0.203071,0.744685,0.826293,0.361293,0.178015,0.488805,0.755665,0.348195,0.215889,0.201953,0.64704,0.179069,0.15548,0.273606,0.126195,0.289856,0.541418,0.168658,0.257738,0.76631,0.186145,0.612398,0.670871,0.309491,0.718402,0.346432,0.489217,0.878281,0.171054,0.250375,0.580627,0.871536,0.422937,0.737356,0.153521,0.204014,0.716327,0.357484,0.206492,0.803474,0.316567,0.800987,0.698455,0.844251,0.174778,0.204577,0.401987,0.256597,0.468467,0.813211,0.361107,0.125191,0.39602,0.77333,0.87517,0.361228,0.79903,0.849401,0.174061,0.589653,0.267215,0.141782,0.82595,0.842971,0.363107,0.697699,0.875518,0.865309,0.307422,0.226361,0.305928,0.664327,0.72471,0.549037,0.20708,0.215221,0.84181,0.795903,0.636349,0.697466,0.465549,0.423218,0.292889,0.805849,0.577569,0.121411,0.311489,0.179607,0.375537,0.472722,0.332493,0.544655,0.783192,0.203,0.836702,0.480469,0.245342,0.661207,0.629317,0.255098,0.445647,0.835832,0.679501,0.20261,0.821418,0.67397,0.346248,0.821459,0.137002,0.658596,0.798644,0.617974,0.271338,0.411855,0.157974,0.442467,0.819109,0.445877,0.829579,0.129508,0.805749,0.805539,0.824929,0.3501,0.34631,0.809836,0.706926,0.41024,0.815404,0.367408,0.758764,0.202498,0.796713,0.771112,0.160972,0.472887,0.779637,0.614273,0.422438,0.128654,0.730628,0.842543,0.290878,0.49506,0.792366,0.860047,0.31504,0.52433,0.787189,0.12546,0.593499,0.294972,0.422457)
tau = c(0.868261,0.662703,0.490565,0.447567,0.581529,0.847664,0.142785,0.575222,0.368389,0.30808,0.697084,0.705546,0.532452,0.477909,0.816346,0.845497,0.126579,0.747481,0.843558,0.6115,0.305692,0.829084,0.708466,0.467555,0.285004,0.612328,0.245483,0.371664,0.383757,0.852818,0.825675,0.187073,0.84239,0.475836,0.175631,0.590872,0.83818,0.223862,0.831463,0.79503,0.880394,0.503392,0.529186,0.164866,0.872421,0.483612,0.715992,0.432457,0.180979,0.163817,0.814418,0.880719,0.409446,0.239772,0.710278,0.132339,0.326341,0.208451,0.346078,0.400277,0.715359,0.226224,0.819568,0.233022,0.499152,0.175426,0.535637,0.660902,0.123338,0.385748,0.130874,0.674397,0.363686,0.272345,0.253391,0.528666,0.651378,0.15575,0.607841,0.254747,0.269803,0.442744,0.174755,0.342803,0.451359,0.247724,0.167379,0.190387,0.383044,0.655781,0.377201,0.710561,0.856755,0.444516,0.435994,0.54878,0.744788,0.865949,0.556687,0.868009,0.850085,0.338342,0.506085,0.431758,0.627134,0.16646,0.605538,0.153508,0.856039,0.495319,0.354991,0.19933,0.640195,0.515175,0.741085,0.864445,0.16023,0.31682,0.844783,0.636326,0.777202,0.707457,0.166033,0.755941,0.778016,0.569628,0.154414,0.467565,0.119478,0.3345,0.812239,0.516227,0.191945,0.126846,0.683754,0.742275,0.727807,0.392227,0.608896,0.656423,0.175319,0.679632,0.455762,0.681104,0.473745,0.640435,0.750275,0.80858,0.819795,0.342943,0.676679,0.875365,0.140659,0.342986,0.4674,0.246923,0.532618,0.345885,0.203641,0.848261,0.377945,0.206872,0.848969,0.765687)
alpha = c(0.690711,0.757814,0.139235,0.386016,0.139875,0.671124,0.305547,0.732619,0.748864,0.374637,0.399323,0.796268,0.188208,0.794837,0.34255,0.386058,0.490686,0.832841,0.705729,0.411191,0.727911,0.229502,0.396932,0.189592,0.835134,0.786862,0.804432,0.597161,0.425689,0.20631,0.156854,0.259337,0.599997,0.149369,0.65448,0.63672,0.47466,0.297956,0.135881,0.166933,0.788539,0.363248,0.737413,0.867964,0.160237,0.382922,0.787134,0.372862,0.298132,0.285519,0.787341,0.820964,0.371925,0.847354,0.734105,0.484636,0.353806,0.181608,0.259646,0.749088,0.551723,0.656525,0.204393,0.83472,0.184987,0.623513,0.248079,0.199693,0.659676,0.853573,0.253127,0.363075,0.294946,0.433272,0.268424,0.531499,0.675172,0.576041,0.735614,0.156448,0.862666,0.678835,0.611733,0.187631,0.271296,0.802804,0.715886,0.492987,0.731694,0.255042,0.800863,0.339539,0.161313,0.292101,0.800982,0.660269,0.319511,0.242936,0.776518,0.624529,0.564217,0.788251,0.332089,0.689341,0.565735,0.671934,0.512003,0.132951,0.49026,0.71136,0.610237,0.860253,0.139232,0.699104,0.29045,0.196083,0.787531,0.166957,0.840773,0.507178,0.503815,0.537321,0.280467,0.254128,0.213375,0.387572,0.306222,0.4503,0.82939,0.348209,0.137527,0.79594,0.16056,0.301166,0.555847,0.241584,0.233523,0.248844,0.172971,0.717944,0.396648,0.165227,0.133777,0.855942,0.713034,0.621742,0.797959,0.691377,0.751604,0.374562,0.222136,0.186443,0.494591,0.831259,0.582714,0.846187,0.718248,0.51612,0.75194,0.784749,0.292574,0.161763,0.491997,0.221505,0.380586,0.725511,0.616432,0.290109,0.799361,0.633508,0.311367,0.701461,0.411238,0.580516,0.73531,0.297807,0.828757,0.854056,0.605909,0.795878,0.6587,0.53796,0.82211,0.157979,0.862511,0.235897,0.493413,0.221243,0.171458,0.836394,0.166832,0.384143,0.445524,0.842253,0.228373,0.699908,0.429083,0.766107,0.810764,0.319546,0.800633,0.647086,0.594929,0.8195,0.274571,0.820231,0.775317,0.598334,0.404251,0.850805,0.685172,0.32786,0.539007,0.366573,0.682415,0.578201,0.83706,0.589834,0.465875,0.808455,0.807607,0.485479,0.434771,0.519658,0.535754,0.411135,0.704079,0.590128,0.221722,0.275809,0.587027,0.593612,0.231076,0.498825,0.413756,0.586038,0.840436,0.210265,0.6084,0.279546,0.134618,0.826372,0.829122,0.871571,0.22677,0.30628,0.704259,0.703043,0.230631,0.614096,0.542284,0.761338,0.803681,0.672399,0.520366,0.855179,0.412369,0.859078,0.860523,0.209464,0.773531,0.473208,0.370616,0.175224,0.14135,0.388431,0.578475,0.277461,0.8402,0.13164,0.649234,0.610213,0.851219,0.81723,0.478421,0.768461,0.202764,0.801666,0.868123,0.189896,0.243472,0.159773,0.27501,0.512293,0.310319,0.369954,0.357927,0.397296,0.559435,0.132494,0.697291,0.863999,0.282988,0.780544)

for(subj in 1:dataList$Nsubj){
p_a_logit=goal_sub_hier_space(hypers[1], #w1_mean
                              hypers[2], #w1_sd,
                              w1,
                              hypers[3],
                              hypers[4],
                              w2,
                              hypers[5],
                              hypers[6],
                              w3,
                              delta,
                              tau,
                              alpha,
                              dataList$Nobs,
                              subj,
                              dataList$expt,
                              dataList$s_sg,
                              dataList$s_tg,
                              dataList$a_logd,
                              dataList$b_logd,
                              dataList$a_logt,
                              dataList$b_logt,
                              dataList$a_dot,
                              dataList$b_dot,
                              dataList$a_tod,
                              dataList$b_tod)
p_a = 1/(1+exp(-p_a_logit))

print(sum(is.na(p_a)))

}



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
                                      dataList$s_sg,
                                      dataList$s_tg,
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

  dsdl_pp = pp_plot_data %>%
    filter(expt==3) %>%
    ungroup() %>%
    mutate(a_t0 = factor(a_t0,labels=paste("Right:",c(1,2,4,8),c("month","months","months","months"))),
           b_t0 = factor(b_t0,labels=paste("Left:",c(1,2,4,8),c("month","months","months","months")))) %>%
    ggplot(data=,aes(x=factor(200-a_d0),colour=factor(200-b_d0))) +
    #geom_ribbon(aes(ymin=y_pred_lo,ymax=y_pred_hi),fill="skyblue") +
    geom_point(aes(y=y_pred_mean),position=position_dodge(width=0.5)) +
    geom_errorbar(aes(ymin=y_pred_lo,ymax=y_pred_hi),width=0.1,position=position_dodge(width=0.5)) +
    geom_point(aes(y=y_obs_mean),shape=2,position=position_dodge(width=0.5)) +
    facet_grid(b_t0~a_t0) +
    coord_cartesian(ylim=c(0,1)) +
    labs(x="Right Starting Height (cm)",y=" ",colour="Left Starting Height (cm)") +
    theme(legend.position="bottom")


  return(list(ds_pp,dl_pp,dsdl_pp))
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
  #ap_dataList=read_rdump(paste0('data/clean/',source,'_ap_rdump.R'))  #Approach
  #av_dataList=read_rdump(paste0('data/clean/',source,'_av_rdump.R'))  #Avoidance

  ap_dataList=read_rdump(paste0('data/clean/',source,'_ap_rdump_expt3.R'))  #Approach
  av_dataList=read_rdump(paste0('data/clean/',source,'_av_rdump_expt3.R'))  #Avoidance

  for (structure in c('hier')){
    for (model in c('space')){

      #Approach
      #load(paste0("data/derived/ap_",source,"_",structure,"_",model,"_fit.RData"))

      load(paste0("data/derived/expt3_ap_fit.RData"))

      ap_pp_data=generate_pp(fit=fit,dataList=ap_dataList,Nsamp=100,version=paste(structure,model,sep='_' ))
      ap_pp_plot= generate_pp_plot(ap_pp_data)

      #Avoidance
      #load(paste0("data/derived/av_",source,"_",structure,"_",model,"_fit.RData"))

      load(paste0("data/derived/expt3_av_fit.RData"))
      av_pp_data=generate_pp(fit=fit,dataList=av_dataList,Nsamp=100,version=paste(structure,model,sep='_' ))
      av_pp_plot= generate_pp_plot(av_pp_data)

      #Posterior predictives
      # pp_fig = arrangeGrob(
      #     arrangeGrob(ap_pp_plot[[1]] + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
      #                 top=textGrob(expression(italic("Experiment 1, Approach Condition")),gp=gpar(fontsize=12))),
      #     arrangeGrob(av_pp_plot[[1]]  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
      #                 top=textGrob(expression(italic("Experiment 1, Avoidance Condition")),gp=gpar(fontsize=12))),
      #     arrangeGrob(ap_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
      #                 top=textGrob(expression(italic("Experiment 2, Approach Condition")),gp=gpar(fontsize=12))),
      #     arrangeGrob(av_pp_plot[[2]]  + theme(axis.text.x = element_text(size=6) , strip.text.x = element_text(size=10)),
      #                 top=textGrob(expression(italic("Experiment 2, Avoidance Condition")),gp=gpar(fontsize=12))),
      #     nrow=4,
      #     left="Proportion Prioritizing Right-hand Goal"
      #   )
      #
      #ggsave(file=paste0("figures/predictives_expt12_",source,"_",structure,"_",model,".png"),plot=pp_fig,width=11,height=10)

      pp_fig = arrangeGrob(
          arrangeGrob(ap_pp_plot[[3]] + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10),legend.position="none"),
                      top=textGrob(expression(italic("Experiment 3, Approach Condition")),gp=gpar(fontsize=12))),
          arrangeGrob(av_pp_plot[[3]]  + theme(axis.text.x = element_text(size=6), strip.text.x = element_text(size=10)),
                      top=textGrob(expression(italic("Experiment 3, Avoidance Condition")),gp=gpar(fontsize=12))),
          nrow=2,
          left="Proportion Prioritizing Right-hand Goal",
          heights=c(1,1.1)
        )
      ggsave(file=paste0("figures/predictives_expt3_",source,"_",structure,"_",model,".png"),plot=pp_fig,width=11,height=10)

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
