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
  real <lower = -0.5,upper = 0.5> r_alpha[n_origin - 3];
  real <lower=-3,upper = 0> r_beta[n_dev - 1];
  real <lower=0.001> elr[n_origin];
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
  alpha[2] = 0;
  alpha[3] = 0;
  alpha[4] = 0;
  
  for (i in 5:n_origin){
    alpha[i] = r_alpha[i-3];
  }
  for (i in 1:(n_dev - 1)){
    beta[i] = r_beta[i];
  }
  
  beta[n_dev] = 0;
  
  for (i in 1:n_dev){
    sig2[i] = sum(a[i:n_dev]);
  }
  
  loglossratio[1] = -0.2873;
  
  for (i in 1:n_origin){
    
    loglossratio[i] = log(elr[i]);
  }
  
  mu[1] = logprem[1] + loglossratio[1] + beta[dev[1]];
  for (i in 2:len_data){
    mu[i] = logprem[i] + loglossratio[origin[i]] + alpha[origin[i]] +  beta[dev[i]] +
      rho*(logloss[origin[i-1]] - mu[origin[i-1]]);
  }
  
  
}

model{
  
  // log loss ratio priors
  
  //elr[1] ~ uniform(-0.2873, -0.2872;
  elr[2] ~ lognormal(-0.30441425, 0.000005);
  elr[3] ~ lognormal(-0.33156664, 0.000005);
  elr[4] ~ lognormal(-0.313342, 0.001);
  elr[5] ~ lognormal(-0.21909167, 0.008);
  elr[6] ~ lognormal(-0.1627, 0.025);
  elr[7] ~ lognormal(-0.378786, 0.035);
  elr[8] ~ lognormal(-0.2393, 0.05);
  elr[9] ~ lognormal(-0.3527, 0.08);
  elr[10] ~ lognormal(-0.3005, 0.08);
  elr[11] ~ lognormal(-0.4339, 0.08);
                     
                     
 for(i in 1:(n_origin - 4)){
 r_alpha[i] ~ uniform(-0.6,0.6);
 }
 r_beta ~ uniform(-3,0);
 a ~ beta(1,7);
 rho ~ uniform(-1,1);
               
 for (i in 1:(len_data)){
 logloss[i] ~ normal(mu[i], sqrt(sig2[dev[i]]));
 }
                    
}
