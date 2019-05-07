functions {
  vector goal_sub(vector phi,          //the sequence of parameters shared across shards,
                  vector theta,        //the sequence of parameters specific to this shard,
                  real[] real_data,    //sequence of real-valued data
                  int[] int_data )     //sequence of integer data
                  {

    //get number of valid observations for that subject
    int Nvalid = int_data[1];
    int Nplaces = int_data[2]; //total number of elements in array (including padding)

    //initialise matrix of data (dat), vector of weights, logits, and an array of
    //observations (y) for each subject.
    matrix[3,Nvalid] dat;
    row_vector[3] weights;
    row_vector[Nvalid] p_a_logit;
    int y_sub[Nvalid];

    //initialize gradient parameters and variables
    real delta_sub = theta[4];
    real tau_sub = theta[5];
    real alpha_sub = theta[6];
    real one_m_alpha_sub = 1 - alpha_sub;
    real a_sg;
    real b_sg;
    real a_tg;
    real b_tg;
    real a_stg_tmp;
    real b_stg_tmp;
    real a_stg;
    real b_stg;

    //initialize log probability variable
    real lp;

    //uncenter and fill weight parameters
    weights[1] = phi[1] + phi[2]*theta[1]; //w1
    weights[2] = phi[3] + phi[4]*theta[2]; //w2
    weights[3] = (phi[5] + phi[6]*theta[3])*2*one_m_alpha_sub*sqrt(alpha_sub / one_m_alpha_sub); //w3*max_spatial

    for(i in 1:Nvalid){

      //calculate difference in spatial gradient
      a_sg = exp(real_data[i]*delta_sub);  //implies a_d ^ delta
      b_sg = exp(real_data[Nplaces+i]*delta_sub); //implies b_d ^ delta
      dat[1,i] = a_sg-b_sg;

      //calculate difference in temporal gradient
      a_tg = exp(real_data[2*Nplaces+i]*tau_sub); //implies a_t ^ tau
      b_tg = exp(real_data[3*Nplaces+i]*tau_sub); //implies b_t ^ tau
      dat[2,i] = a_tg-b_tg;

      //calculate difference in spatiotemporal gradient
      a_stg_tmp = alpha_sub * real_data[6*Nplaces+i] + one_m_alpha_sub * real_data[4*Nplaces+i];
      b_stg_tmp = alpha_sub * real_data[7*Nplaces+i] + one_m_alpha_sub * real_data[5*Nplaces+i];
      a_stg = inv(a_stg_tmp);
      b_stg = inv(b_stg_tmp);
      dat[3,i] = a_stg - b_stg;

    }

    //matrix multiplication to calculate the logit for each observation for that subject
    p_a_logit = weights * dat;

    lp = bernoulli_logit_lpmf(int_data[3:(Nvalid+2)] | p_a_logit);

    return [lp]';
  }
}

data {
  int<lower=0> Ntotal;
  int<lower=0> Nsubj;
  int<lower=0> Max_obs;
  real real_data[Nsubj,Max_obs*8];
  int int_data[Nsubj,Max_obs+2];
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

transformed parameters {
  vector[12] phi;
  vector[6] theta[Nsubj];

  phi[1] = w1_mean;
  phi[2] = w1_sd;
  phi[3] = w2_mean;
  phi[4] = w2_sd;
  phi[5] = w3_mean;
  phi[6] = w3_sd;
  phi[7] = delta_mean;
  phi[8] = delta_sd;
  phi[9] = tau_mean;
  phi[10] = tau_sd;
  phi[11] = alpha_mean;
  phi[12] = alpha_sd;

  for(subj in 1:Nsubj){

    if(expt[subj] == 1){
      theta[subj,1] = w1[s_sg[subj]];
      theta[subj,2] = 0;
      theta[subj,3] = w3[subj];
      theta[subj,4] = delta[s_sg[subj]];
      theta[subj,5] = 0;
      theta[subj,6] = alpha[subj];
    }

    if(expt[subj] == 2){
      theta[subj,1] = 0;
      theta[subj,2] = w2[s_tg[subj]];
      theta[subj,3] = w3[subj];
      theta[subj,4] = 0;
      theta[subj,5] = tau[s_tg[subj]];
      theta[subj,6] = alpha[subj];
    }

    if(expt[subj] == 3){
      theta[subj,1] = w1[s_sg[subj]];
      theta[subj,2] = w2[s_tg[subj]];
      theta[subj,3] = w3[subj];
      theta[subj,4] = delta[s_sg[subj]];
      theta[subj,5] = tau[s_tg[subj]];
      theta[subj,6] = alpha[subj];
    }
  }
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



 for(subj_sg in 1:Nsubj_sg){
   delta[subj_sg] ~ normal(delta_mean,delta_sd) T[0,1];
 }
 for(subj_tg in 1:Nsubj_tg){
   tau[subj_tg] ~ normal(tau_mean,tau_sd) T[0,1];
 }

  //loop through subjects evaluating likelihood for each one
  for(subj in 1:Nsubj){
    alpha[subj] ~ normal(alpha_mean,alpha_sd) T[0.01,0.99];
  }

  target += sum(map_rect(goal_sub,phi,theta,real_data,int_data));

}


