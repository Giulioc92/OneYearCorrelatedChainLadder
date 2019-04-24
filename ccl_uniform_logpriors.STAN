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
  real <lower= -0.361, upper = -0.35 > logelr2;
  real <lower=-0.1, upper = -0.04 > logelr3;
  real <lower=-0.1, upper = -0.04 > logelr4;
  real <lower=0.05, upper = 0.2 > logelr5;  
  real <lower=0.05, upper = 0.22 > logelr6;
  real <lower=-0.304, upper = 0 > logelr7;
  real <lower=-0.32, upper = -0.2 > logelr8;
  real <lower=-0.43, upper = 0.18 > logelr9;  
  real <lower=-0.43, upper = 0.18 > logelr10;
  real <lower=-0.43, upper = 0.18 > logelr11;
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
  loglossratio[2] = logelr2;
  loglossratio[3] = logelr3;
  loglossratio[4] = logelr4;
  loglossratio[5] = logelr5;
  loglossratio[6] = logelr6;
  loglossratio[7] = logelr7;
  loglossratio[8] = logelr8;
  loglossratio[9] = logelr9;
  loglossratio[10] = logelr10;
  loglossratio[11] = logelr11;
  
  mu[1] = logprem[1] + loglossratio[1] + beta[dev[1]];
  for (i in 2:len_data){
    mu[i] = logprem[i] + loglossratio[origin[i]] + alpha[origin[i]] +  beta[dev[i]] +
      rho*(logloss[origin[i-1]] - mu[origin[i-1]]);
  }
  
  
}

model{
  
  // log loss ratio priors
  
  //logelr[1] ~ uniform(-0.431, -0.429);
  logelr2 ~ uniform(-0.36,-0.35);
  
  logelr3 ~ uniform(-0.10, -0.04);
  logelr4 ~ uniform(-0.10, -0.04);
  
  logelr5 ~ uniform(0.05, 0.20);
  logelr6 ~ uniform(0.05, 0.20);
  
  logelr7 ~ uniform(-0.304,0);
  logelr8 ~ uniform(-0.32,-0.2);
  
  logelr9 ~ uniform(-0.43, 0.18);
  logelr10 ~ uniform(-0.43, 0.18);
  logelr11 ~ uniform(-0.43, 0.18);
  
  
  r_alpha ~ normal(0, 0.05);
  r_beta ~ uniform(-3,0);
  a ~ beta(1,1);
  rho ~ uniform(-1,1);
  
  for (i in 1:(len_data)){
    logloss[i] ~ normal(mu[i], sqrt(sig2[dev[i]]));
  }
  
}
