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

    //If expt == 3, both w1 and w2 are relevant
    weights[1] = w1_mean + w1[subj]*w1_sd;
    weights[2] = w2_mean + w2[subj]*w2_sd;
    delta_sub = delta[subj];
    tau_sub = tau[subj];

    //the 3rd columns of data and weights are the same for all subjects.
    alpha_sub = alpha[subj];
    one_m_alpha_sub = 1-alpha_sub;
    weights[3] = (w3_mean + w3[subj]*w3_sd)*2*one_m_alpha_sub*sqrt(alpha_sub/one_m_alpha_sub);

    for(obs in 1:Nobs[subj]){

      a_sg = exp(a_logd[obs,subj]*delta_sub);
      b_sg = exp(b_logd[obs,subj]*delta_sub);
      a_tg = exp(a_logt[obs,subj]*tau_sub);
      b_tg = exp(b_logt[obs,subj]*tau_sub);
      dat[1,obs] = a_sg-b_sg;
      dat[2,obs] = a_tg-b_tg;

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
  real w1[Nsubj];

  real<upper=0> w2_mean;
  real<lower=0> w2_sd;
  real<upper=0> w2[Nsubj];

  real<lower=0> w3_mean;
  real<lower=0> w3_sd;
  real<lower=0> w3[Nsubj];

  real<lower=0,upper=1> delta_mean;
  real<lower=0> delta_sd;
  real<lower=0,upper=1> delta[Nsubj];

  real<lower=0,upper=1> tau_mean;
  real<lower=0> tau_sd;
  real<lower=0,upper=1> tau[Nsubj];

  real<lower=0.01,upper=0.99> alpha_mean;
  real<lower=0> alpha_sd;
  real<lower=0.01,upper=0.99> alpha[Nsubj];

}

model {

  //set hyperpriors
  w1_mean ~ normal(0,5);
  w1_sd ~ normal(0,5);
  w2_mean ~ normal(0,5);
  w2_sd ~ normal(0,5);
  w3_mean ~ normal(0,5);
  w3_sd ~ normal(0,5);

  //gradient means are uniformatlly distributed
  delta_sd ~ normal(0,1);
  tau_sd ~ normal(0,1);
  alpha_sd ~ normal(0,1);

  //set priors
  w1 ~ normal(0,1);  //implies actual w1 ~ normal ( w1_mean, w1_sd)
  w2 ~ normal(0,1);
  w3 ~ normal(0,1);

  //loop through subjects evaluating likelihood for each one
  for(subj in 1:Nsubj){
    row_vector[Nobs[subj]] p_a_logit;

    delta[subj] ~ normal(delta_mean,delta_sd) T[0,1];
    tau[subj] ~ normal(tau_mean,tau_sd) T[0,1];
    alpha[subj] ~ normal(alpha_mean,alpha_sd) T[0.01,0.99];

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


