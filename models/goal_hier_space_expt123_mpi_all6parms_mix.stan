//TODO: 1) Phi no longer needs to feed into map_rect, 2) One parameter needs to be an ordered vector (w3?)

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
  int<lower=0>Nmix; //number of mixtures
}

parameters {

  real w1_mean[Nmix];
  real<lower=0> w1_sd[Nmix];
  real w1[Nsubj];

  real<upper=0> w2_mean[Nmix];
  real<lower=0> w2_sd[Nmix];
  real<upper=0> w2[Nsubj];

  real<lower=0> w3_mean[Nmix];
  real<lower=0> w3_sd[Nmix];
  real<lower=0> w3[Nsubj];

  real<lower=0,upper=1> delta_mean[Nmix];
  real<lower=0> delta_sd[Nmix];
  real<lower=0,upper=1> delta[Nsubj];

  real<lower=0,upper=1> tau_mean[Nmix];
  real<lower=0> tau_sd[Nmix];
  real<lower=0,upper=1> tau[Nsubj];

  real<lower=0.01,upper=0.99> alpha_mean[Nmix];
  real<lower=0> alpha_sd[Nmix];
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

      theta[subj,1] = w1[subj];
      theta[subj,2] = w2[subj];
      theta[subj,3] = w3[subj];
      theta[subj,4] = delta[subj];
      theta[subj,5] = tau[subj];
      theta[subj,6] = alpha[subj];

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
  // w1 ~ normal(0,1);  //implies actual w1 ~ normal ( w1_mean, w1_sd)
  // w2 ~ normal(0,1);
  // w3 ~ normal(0,1);

  //loop through subjects evaluating likelihood for each one
  for(k in 1:Nmix){
    for(subj in 1:Nsubj){

      //re-centre parms
      w1_tmp = w1_mean[k] + w1[subj]*w1_sd[k];
      w1_tmp = w2_mean[k] + w2[subj]*w2_sd[k];
      w1_tmp = w3_mean[k] + w3[subj]*w3_sd[k];

       lps[k] = log(mix_weight[subj,k]) + normal_lpdf(w1[subj]  | w1_mean[k],w1_sd[k]) +
         log(mix_weight[subj,k]) + normal_lpdf(w2[subj]  | w2_mean[k], w2_sd[k]) T[0,] +
         log(mix_weight[subj,k]) + normal_lpdf(w3[subj]  | w3_mean[k], w3_sd[k]) T[0,] +
         log(mix_weight[subj,k]) + normal_lpdf(delta[subj]  |delta_mean[k],delta_sd[k]) T[0,1] +
         log(mix_weight[subj,k]) + normal_lpdf(tau[subj]   |tau_mean[k],tau_sd[k]) T[0,1] +
         log(mix_weight[subj,k]) + normal_lpdf(alpha[subj] | alpha_mean[k],alpha_sd[k]) T[0.01,0.99];
    }

    target += log_sum_exp(lps);
  }

  target += sum(map_rect(goal_sub,phi,theta,real_data,int_data));

    //LIKELIHOOD
  //loop through all trials in the dataset performing bracketed operations on each one
  // for(i in 1:Ntotal){
  //   //initialize object to store weighted logged likelihood of observed goal for each mixture
  //   real lps[Nmix];
  //   for(k in 1:Nmix){
  //     //if the trial being considered is the first trial for that subject...
  //     if(trial[i]==1){
  //       //for each mixture, set predicted_goal to be equal to observed_goal for that trial
  //       predicted_goal[k] = observed_goal[i];
  //     }
  //     if(trial[i]>1){
  //       //for each mixture, increment predicted_goal according to the theory of change, using parameters relevant to that mixture.
  //       predicted_goal[k] += alpha[k]*(performance[i-1]-predicted_goal[k]) + beta[k];
  //     }
  //     //calculated weighted log likelihood of observed goal under mixture being considered
  //     lps[k] = log(mix_weight[subject[i],k]) + normal_lpdf(observed_goal[i] | predicted_goal[k], sigma);
  //   }
  //    //sum weighted log likelihoods to get the logged likelihood of observation given combination of mixtures
  //   target += log_sum_exp(lps);
  // }

}


