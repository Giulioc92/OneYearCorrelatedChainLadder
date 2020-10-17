//############## stan.model ######################
data{
  int <lower=1> ndata; // number of rows with data
  int<lower=0, upper=1> origin1id[ndata]; 
  int<lower=1> obs;
  real logprem[ndata]; 
  real logloss[ndata];
  int<lower=1> origin[ndata]; // origin period
  int<lower=1> dev[ndata]; // development period
}

transformed data{
  int norigin = max(origin);
  int ndev = max(dev);
}
parameters{
  real <lower = -0.5,upper = 0.5> ralpha[norigin - 3];
  real <lower=-3,upper = 0> rbeta[ndev - 1];
  real <lower=0.001> elr[norigin];
  //real<lower=0> a[n_dev];
  real<lower=-1, upper=1> rho;
  real <lower=0,upper=1> a[ndev];
}

transformed parameters{
  real mu[ndata];
  real alpha[norigin];
  real beta[ndev];
  real sig2[ndev];
  real loglossratio[norigin];
  //real sig[n_dev];
  //real <lower=-1, upper=1> rho;
  
  //rho = -2*rho_r + 1;
  
  //real sigma_trasf[n_dev] = sig2[n_dev]; 
  
  alpha[1] = 0;
  alpha[2] = 0;
  alpha[3] = 0;
  alpha[4] = 0;
  
  for (i in 5:norigin){
    alpha[i] = ralpha[i-3];
  }
  for (i in 1:(ndev - 1)){
    beta[i] = rbeta[i];
  }
  
  beta[ndev] = 0;
  
  for (i in 1:ndev){
    sig2[i] = sum(a[i:ndev]);
  }
  
  loglossratio[1] = -0.2873;
  
  for (i in 1:norigin){
    
    loglossratio[i] = log(elr[i]);
  }
  
  mu[1] = logprem[1] + loglossratio[1] + beta[dev[1]];
  for (i in 2:ndata){
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
                     
                     
 for(i in 1:(norigin - 4)){
 ralpha[i] ~ uniform(-0.6,0.6);
 }
 rbeta ~ uniform(-3,0);
 a ~ beta(1,7);
 rho ~ uniform(-1,1);
               
 for (i in 1:(ndata)){
 logloss[i] ~ normal(mu[i], sqrt(sig2[dev[i]]));
 }
                    
}
