//############## stan.model ######################
data{
  int <lower=1> len_data; // number of rows with data
  int<lower=0, upper=1> origin1id[len_data]; 
  int<lower=1> l_a;
  real logprem[len_data]; 
  real logloss[len_data];
  int<lower=1> origin[len_data]; // origin period
  int<lower=1> dev[len_data]; // development period
}

transformed data{
  int n_origin = max(origin);
  int n_dev = max(dev);
}
parameters{
  real r_alpha[n_origin - 1];
  real <lower=-3,upper = 0> r_beta[n_dev - 1];
  //Poor coding style but looping over variables declaration seems not to be allowed
  real <lower=0.697, upper = 0.705 > elr2;
  real <lower=0.905, upper = 0.96 > elr3;
  real <lower=0.905, upper = 0.96 > elr4;
  real <lower=1.05, upper = 1.22 > elr5;  
  real <lower=1.05, upper = 1.22 > elr6;
  real <lower=0.738, upper = 1 > elr7;
  real <lower=0.726, upper = 0.819 > elr8;
  real <lower=1.05, upper = 1.22 > elr9;  
  real <lower=1.05, upper = 1.22 > elr10;
  real <lower=1.05, upper = 1.22 > elr11;
  //real<lower=0> a[n_dev];
  real<lower=-1, upper=1> rho;
  real <lower=0,upper=1> a[n_dev];
}

transformed parameters{
  real mu[len_data];
  real alpha[n_origin];
  real beta[n_dev];
  real sig2[n_dev];
  real loglossratio[n_origin];
  //real sig[n_dev];
  //real <lower=-1, upper=1> rho;
  
  //rho = -2*rho_r + 1;
  
  //real sigma_trasf[n_dev] = sig2[n_dev]; 
  
  alpha[1] = 0;
  for (i in 2:n_origin){
    alpha[i] = r_alpha[i-1];
  }
  for (i in 1:(n_dev - 1)){
    beta[i] = r_beta[i];
  }
  
  beta[n_dev] = 0;
  
  for (i in 1:n_dev){
    sig2[i] = sum(a[i:n_dev]);
  }
  
  loglossratio[1] = -0.43;
  
    loglossratio[2] = log(elr2);
    loglossratio[3] = log(elr3);
    loglossratio[4] = log(elr4);
    loglossratio[5] = log(elr5);
    loglossratio[6] = log(elr6);
    loglossratio[7] = log(elr7);
    loglossratio[8] = log(elr8);
    loglossratio[9] = log(elr9);
    loglossratio[10] = log(elr10);
    loglossratio[11] = log(elr11);
  
  mu[1] = logprem[1] + loglossratio[1] + beta[dev[1]];
  for (i in 2:len_data){
    mu[i] = logprem[i] + loglossratio[origin[i]] + alpha[origin[i]] +  beta[dev[i]] +
      rho*(logloss[origin[i-1]] - mu[origin[i-1]]);
  }
  
  
}

model{
  
  // log loss ratio priors
  
  //logelr[1] ~ uniform(-0.431, -0.429);
  elr2 ~ uniform(0.697,0.705);
  
  elr3 ~ uniform(0.905, 0.96);
  elr4 ~ uniform(0.905, 0.96);
  
  elr5 ~ uniform(1.05, 1.22);
  elr6 ~ uniform(1.05, 1.22);
  
  elr7 ~ uniform(0.738,1);
  elr8 ~ uniform(0.726,0.819);
  
  elr9 ~ uniform(0.65, 1.2);
  elr10 ~ uniform(0.65, 1.2);
  elr11 ~ uniform(0.65, 1.2);

  
  r_alpha ~ normal(0, 3);
  r_beta ~ uniform(-3,0);
  a ~ beta(1,1);
  rho ~ uniform(-1,1);
  
  for (i in 1:(len_data)){
    logloss[i] ~ normal(mu[i], sqrt(sig2[dev[i]]));
  }
  
}
