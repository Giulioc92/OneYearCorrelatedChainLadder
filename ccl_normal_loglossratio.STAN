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
  real <lower=-0.9,upper=0.25> logelr[n_origin];
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
  
  for (i in 1:n_origin){
    
    loglossratio[i] = logelr[i];
  }
  
  mu[1] = logprem[1] + loglossratio[1] + beta[dev[1]];
  for (i in 2:len_data){
    mu[i] = logprem[i] + loglossratio[origin[i]] + alpha[origin[i]] +  beta[dev[i]] +
      rho*(logloss[origin[i-1]] - mu[origin[i-1]]);
  }
  
  
}

model{
  
  // log loss ratio priors
  
  //logelr[1] ~ uniform(-0.431, -0.429);
  logelr[2] ~ normal(-0.361, 0.1);
  
  for (i in 3:4){
    logelr[i] ~ normal(-0.065, 0.12);
  }
  
  for (i in 5:6){
    logelr[i] ~ normal(0.125, 0.1);
  }
  
  logelr[7] ~ normal(-0.152, 0.5);
  logelr[8] ~ normal(-0.26, 0.5);
  
  for (i in 9:n_origin){
    logelr[i] ~ normal(-0.35, 1);
  }
  
  r_alpha ~ normal(0, 0.1);
  r_beta ~ uniform(-3,0);
  a ~ beta(1,1);
  rho ~ uniform(-1,1);
  
  for (i in 1:(len_data)){
    logloss[i] ~ normal(mu[i], sqrt(sig2[dev[i]]));
  }
  
}
