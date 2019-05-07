functions {
  row_vector goal_sub(real w1_mean,
                      real w1_sd,
                      real[] w1,
                      real w2_mean,
                      real w2_sd,
                      real[] w2,
                      real w3_mean,
                      real w3_sd,
                      real[] w3,
                      real[] delta,
                      real[] tau,
                      real[] alpha,
                      int[] Nobs,
                      int subj,
                      int[] expt,
                      int[] s_sg,
                      int[] s_tg,
                      matrix a_logd,
                      matrix b_logd,
                      matrix a_logt,
                      matrix b_logt,
                      matrix a_dot,
                      matrix b_dot,
                      matrix a_tod,
                      matrix b_tod)
                      {

    //initialize subject parameters
    real delta_sub;
    real tau_sub;
    real alpha_sub;
    real one_m_alpha_sub;
    real a_sg;
    real b_sg;
    real a_tg;
    real b_tg;
    real a_stg;
    real b_stg;

  //initialise matrix of data (dat), vector of weights, logits, and an array of
      //observations (y) for each subject. These need to be re-initialized for every
      //subject because their size will differ for each subject.
      matrix[3,Nobs[subj]] dat;
      row_vector[3] weights;
      row_vector[Nobs[subj]] p_a_logit;
      int y_sub[Nobs[subj]];

    ///The data in the first column and the first weight are conditional on a subject-level variable (expt).
    //If expt == 1, w1 is used as the weights and the data is calculated based on logd and delta.
    if(expt[subj] == 1){
      weights[1] = w1_mean + w1[s_sg[subj]]*w1_sd;
      weights[2] = 0;
      delta_sub = delta[s_sg[subj]];
      for(obs in 1:Nobs[subj]){
        a_sg = exp(a_logd[obs,subj]*delta_sub);
        b_sg = exp(b_logd[obs,subj]*delta_sub);
        dat[1,obs] = a_sg-b_sg;
        dat[2,obs] = 0;
      }
    }
    //If expt == 2, w2 is used as the weights and the data is calculated based on logt and tau.
    if(expt[subj] == 2){
      weights[1] = 0;
      weights[2] = w2_mean + w2[s_tg[subj]]*w2_sd;
      tau_sub = tau[s_tg[subj]];
      for(obs in 1:Nobs[subj]){
        a_tg = exp(a_logt[obs,subj]*tau_sub);
        b_tg = exp(b_logt[obs,subj]*tau_sub);
        dat[1,obs] = 0;
        dat[2,obs] = a_tg-b_tg;
      }
    }

    //If expt == 3, both w1 and w2 are relevant
    if(expt[subj] == 3){
     weights[1] = w3_mean + w3[s_sg[subj]]*w1_sd;
     weights[2] = w3_mean + w3[s_tg[subj]]*w2_sd;
     delta_sub = delta[s_sg[subj]];
     tau_sub = tau[s_tg[subj]];
      for(obs in 1:Nobs[subj]){
        a_sg = exp(a_logd[obs,subj]*delta_sub);
        b_sg = exp(b_logd[obs,subj]*delta_sub);
        a_tg = exp(a_logt[obs,subj]*tau_sub);
        b_tg = exp(b_logt[obs,subj]*tau_sub);
        dat[1,obs] = a_sg-b_sg;
        dat[2,obs] = a_tg-b_tg;
      }
    }

    if(expt[subj] == 3){
     weights[1] = w3_mean + w3[s_sg[subj]]*w1_sd;
     weights[2] = w3_mean + w3[s_tg[subj]]*w2_sd;
     delta_sub = delta[s_sg[subj]];
     tau_sub = tau[s_tg[subj]];
      for(obs in 1:Nobs[subj]){
        a_sg = exp(a_logd[obs,subj]*delta_sub);
        b_sg = exp(b_logd[obs,subj]*delta_sub);
        a_tg = exp(a_logt[obs,subj]*tau_sub);
        b_tg = exp(b_logt[obs,subj]*tau_sub);
        dat[1,obs] = a_sg-b_sg;
        dat[2,obs] = a_tg-b_tg;
      }
    }

    //the 3rd columns of data and weights are the same for all subjects.
    alpha_sub = alpha[subj];
    one_m_alpha_sub = 1-alpha_sub;
    weights[3] = (w3_mean + w3[subj]*w3_sd)*2*one_m_alpha_sub*sqrt(alpha_sub/one_m_alpha_sub);

    for(obs in 1:Nobs[subj]){
      a_stg = 1  / (alpha_sub * a_tod[obs,subj] + one_m_alpha_sub * a_dot[obs,subj]);
      b_stg = 1  / (alpha_sub * b_tod[obs,subj] + one_m_alpha_sub * b_dot[obs,subj]);
      dat[3,obs] = a_stg - b_stg;
    }

    //matrix multiplication to calculate the logit for each observation for that subject
    p_a_logit = weights * dat;
    return p_a_logit;
  }
}

data {
  int<lower=0> Ntotal;
  int<lower=0> Nsubj;
  int<lower=0> Maxobs;
  int<lower=0> Nobs[Nsubj];
  int<lower=0> s[Ntotal];
  int<lower=0,upper=1> y[Maxobs,Nsubj];
  matrix [Maxobs,Nsubj] a_logd;
  matrix [Maxobs,Nsubj] b_logd;
  matrix [Maxobs,Nsubj] a_logt;
  matrix [Maxobs,Nsubj] b_logt;
  matrix [Maxobs,Nsubj] a_dot;
  matrix [Maxobs,Nsubj] b_dot;
  matrix [Maxobs,Nsubj] a_tod;
  matrix [Maxobs,Nsubj] b_tod;
  int expt[Nsubj];
  int s_sg[Nsubj];
  int s_tg[Nsubj];
  int Nsubj_sg;
  int Nsubj_tg;
}

parameters {

  real w1_mean;
  real<lower=0> w1_sd;
  real w1[Nsubj_sg];

  real<upper=0> w2_mean;
  real<lower=0> w2_sd;
  real<upper=0> w2[Nsubj_tg];

  real<lower=0> w3_mean;
  real<lower=0> w3_sd;
  real<lower=0> w3[Nsubj];

  real<lower=0,upper=1> delta_mean;
  real<lower=0> delta_sd;
  real<lower=0,upper=1> delta[Nsubj_sg];

  real<lower=0,upper=1> tau_mean;
  real<lower=0> tau_sd;
  real<lower=0,upper=1> tau[Nsubj_tg];

  real<lower=0.01,upper=0.99> alpha_mean;
  real<lower=0> alpha_sd;
  real<lower=0.01,upper=0.99> alpha[Nsubj];

}

model {

  //set hyperpriors
  w1_mean_ap ~ normal(0,5);
  w1_sd_ap ~ normal(0,5);
  w1_mean_av ~ normal(0,5);
  w1_sd_av ~ normal(0,5);

  w2_mean_ap ~ normal(0,5);
  w2_sd_ap ~ normal(0,5);
  w2_mean_av ~ normal(0,5);
  w2_sd_av ~ normal(0,5);

  w3_mean_ap ~ normal(0,5);
  w3_sd_ap ~ normal(0,5);
  w3_mean_av ~ normal(0,5);
  w3_sd_av ~ normal(0,5);

  //gradient means are uniformatlly distributed
  delta_sd_ap ~ normal(0,1);
  delta_sd_av ~ normal(0,1);

  tau_sd_ap ~ normal(0,1);
  tau_sd_av ~ normal(0,1);

  alpha_sd_ap ~ normal(0,1);
  alpha_sd_av ~ normal(0,1);

  //set priors
  w1_ap ~ normal(0,1);  //implies actual w1 ~ normal ( w1_mean, w1_sd)
  w1_av ~ normal(0,1);  //implies actual w1 ~ normal ( w1_mean, w1_sd)

  w2_ap ~ normal(0,1);
  w2_av ~ normal(0,1);

  w3_ap ~ normal(0,1);
  w3_av ~ normal(0,1);


 for(subj_sg in 1:Nsubj_sg){
   delta_ap[subj_sg] ~ normal(delta_mean_ap,delta_sd_ap) T[0,1];
   delta_av[subj_sg] ~ normal(delta_mean_av,delta_sd_av) T[0,1];
 }
 for(subj_tg in 1:Nsubj_tg){
   tau_ap[subj_tg] ~ normal(tau_mean_ap,tau_sd_ap) T[0,1];
   tau_av[subj_tg] ~ normal(tau_mean_av,tau_sd_av) T[0,1];
 }

  //loop through subjects evaluating likelihood for each one
  for(subj in 1:Nsubj){
    row_vector[Nobs[subj]] p_a_logit;

    alpha_ap[subj] ~ normal(alpha_mean_ap,alpha_sd_ap) T[0.01,0.99];
    alpha_av[subj] ~ normal(alpha_mean_av,alpha_sd_av) T[0.01,0.99];

    p_a_logit = goal_sub(w1_mean,w1_sd,w1,
                         w2_mean,w2_sd,w2,
                         w3_mean,w3_sd,w3,
                         delta,tau,alpha,
                         Nobs,subj,expt,s_sg,s_tg,
                         a_logd,b_logd,a_logt,b_logt,
                         a_dot,b_dot,a_tod,b_tod);


    y[1:Nobs[subj],subj] ~ bernoulli_logit(p_a_logit);
  }
}


