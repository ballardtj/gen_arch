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
    weights[1] = theta[1]; //w1
    weights[2] = theta[2]; //w2
    weights[3] = theta[3]*2*one_m_alpha_sub*sqrt(alpha_sub / one_m_alpha_sub); //w3*max_spatial

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

  real w1[Nsubj_sg];
  real<upper=0> w2[Nsubj_tg];
  real<lower=0> w3[Nsubj];
  real<lower=0.2,upper=1> delta[Nsubj_sg];
  real<lower=0.2,upper=1> tau[Nsubj_tg];
  real<lower=0.01,upper=0.99> alpha[Nsubj];

}

transformed parameters {
  vector[1] phi;
  vector[6] theta[Nsubj];

  phi[1] = 1;

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

  //set priors
  w1 ~ normal(0,5);  //implies actual w1 ~ normal ( w1_mean, w1_sd)
  w2 ~ normal(0,5);
  w3 ~ normal(0,5);

  target += sum(map_rect(goal_sub,phi,theta,real_data,int_data));

}


