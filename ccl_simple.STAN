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
  real <lower=-1,upper=0.5> logelr;
  //real<lower=0> a[n_dev];
  real<lower=-1, upper=1> rho;
  real <lower=0,upper=1> a[n_dev];
}

transformed parameters{
  real mu[len_data];
  real alpha[n_origin];
  real beta[n_dev];
  real sig2[n_dev];
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
  
  mu[1] = logprem[1] + logelr + beta[dev[1]];
  for (i in 2:len_data){
    mu[i] = logprem[i] + logelr + alpha[origin[i]] +  beta[dev[i]] +
      rho*(logloss[origin[i-1]] - mu[origin[i-1]]);
  }
  
  
}

model{
  
  logelr ~ uniform(-0.8, 0.3);
  r_alpha ~ normal(0, 3);
  r_beta ~ uniform(-3,0);
  a ~ beta(1,1);
  rho ~ uniform(-1,1);
  
  for (i in 1:(len_data)){
    logloss[i] ~ normal(mu[i], sig2[dev[i]]);
  }
  
}
