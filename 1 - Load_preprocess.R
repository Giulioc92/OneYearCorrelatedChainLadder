##### 1 - Load packages and load data ##### 
library(rstan)
library(dplyr)
library(ggplot2)
library(ChainLadder)
library(tictoc)
library(profvis)
library(microbenchmark)
library(moments)
library(parallel)
library(readxl)
library(ggmcmc)

############# Import the data (a cumulative triangle)
data <- read.table('mtpl_triangle.txt')

############# Manipulation
## log transformation of cumulative and premiums
## flagging with 1 the first accident year

cdat <- data %>% mutate(logloss = log(cumulative),
                        logprem = log(premiums), 
                        origin1id = ifelse(origin == 1,0,1)) %>%
  select(origin,dev,logloss,logprem, origin1id)

## setup a stan datalist for STAN

stan_data <- list(
  len_data <- dim(cdat)[1],
  logprem = cdat$logprem,
  logloss = cdat$logloss,
  origin = cdat$origin,
  dev = cdat$dev,
  l_a = (max(cdat$origin) * (max(cdat$origin)+1))/2,
  origin1id = cdat$origin1id
)

### visualize the list 

stan_data

### 1.1 explore the triangle

tri <- data %>% mutate(value = cumulative) %>% as.triangle(origin = 'origin',dev = 'dev',value = 'value')

t <- cdat %>% select(dev) %>% max
####
plot(tri,lattice = T)
ata(tri)
Mack_CL <- MackChainLadder(tri, est.sigma = 'Mack')

#### Mack implied loss ratio

mack <- MackChainLadder(tri, est.sigma = 'Mack') %>% summary
mack_lr <- (mack$ByOrigin$Ultimate / unique(data$premiums)) %>% cbind(origin = 1:t) %>%  as.data.frame
colnames(mack_lr)[1] <- 'lr'
#### get some kind of credibility of Mack LR
options(digits = 8)
mack_lr$dev_to_date <- mack$ByOrigin$Dev.To.Date %>%  unlist
### get the diagonal
mack_lr$latest <- mack$ByOrigin$Latest %>% unlist
mack_lr$ultimate <- mack$ByOrigin$Ultimate %>% unlist
mack_lr$loglr <- log(mack_lr$lr)
mack_lr$to_pay <- 1 - mack_lr$dev_to_date
mack_lr$lr_paid <- mack_lr$latest / unique(data$premiums)
mack_lr

#### fit a log normal distribution on the loss ratio
#### to get a reference point on the loss ratio parameter space

lr_dist_param <- fitdistrplus::fitdist(mack_lr$lr,distr = 'lnorm',method = 'mle')
exp(lr_dist_param[[1]][1]+lr_dist_param[[1]][1]**2/2) 
sigma_emp <- lr_dist_param[[1]][2]

#### the hypothesis for the prior loss ratio is log normal,
#### I'd like to know, for a given loss ratio mean and a sigma, the mu parameters
#### that could be used to generate the logarithm of this loss ratio from a normal distribution,
#### or to generate sample from a log normal distribution centered on a particular value
#### (assuming a fixed sigma, estimate the mu of the lognormal that would imply the Mack ultimate costs)

find_the_mu <- function(mean,sigma, lognormal = T){
  if(lognormal == T){
  mu = log(mean) - (sigma**2)/2
  } else {
  mu = mean - (sigma**2)/2  
  }
return(mu)
  }

### get the mean when sigma to be squared
ln_mean <- function(mu,sigma){
  mean = exp(mu + (sigma**2)/2)
  return(mean)
}
#### benchmark to mix with prior information about loss ratio

#### A normal distribution is chosen for prior loss ratio
#### I decide to set a mean value for the prior loss ratio,based on my evaluations
#### there could be studied a relationship between the dev_to_date and prior st dev

#### I calculate the average loss ratio implied by this triangle

(mean_lossratio <- mack_lr$lr %>% mean)

lr_priors <- mack_lr %>% mutate(ibnr = ultimate - latest,lr_to_be_paid = (lr-lr_paid)/lr) %>% 
  select(ultimate,latest,ibnr,lr_paid,lr,lr_to_be_paid)
lr_priors$prior_lr <- 0 
  
lr_priors[2,7] <- lr_priors[2,5]
lr_priors[3,7] <- lr_priors[3,5]

#### untouched, we trust Mack, not real variation on the final loss ratio
#### The idea is to trust less and less Mack implied loss ratio and more our
#### external knowledge
#### the more is left to pay the more we make room for uncertainty

lr_priors[4,7] <- .731
lr_priors[5,7] <- .806
lr_priors[6,7] <- .851  ### lr_priors[6,5] * (1.02)
lr_priors[7,7] <- .685  ### lr_priors[7,5] * (1.02)
lr_priors[8,7] <- .787
lr_priors[9,7] <- .705

#### particular care for the two youngest generations
#### implied loss ratio are lower than long-term loss ratio of the portfolio
#### I converge to that value

lr_priors[10,7] <- .743

### the last loss ratio in particular seems not to
### be in line with that ot immediately preceding generations:
### great uncertainty and considerably lower loss ratios
### make us choose a wider prior, centered on higher values than the mack ones
lr_priors[11,7] <- .65 

lr_priors$premiums <- data$premiums %>% unique

options(digits = 8)
lr_priors %>% mutate(d_to_d = mack_lr$dev_to_date,prior_ult = prior_lr*premiums,
                               'prior > lat' = prior_ult > latest) %>% 
  select(latest,ultimate,d_to_d, lr, prior_ult,prior_lr,'prior > lat')

### and check that the final loss ratio is higher than the paid loss-ratio
### (assumption of no more recoverables)
### get the parameters for a log-normal loss ratio to feed the MCMC sampler

sapply(lr_priors$prior_lr,function(p) find_the_mu(p,sigma_emp)) %>% unname

log_priors <- lr_priors %>% mutate(log_mu_prior = find_the_mu(prior_lr,sigma_emp),
                                   log_mack_lr = find_the_mu(lr,sigma_emp),to_be_perc = (lr_to_be_paid)/(lr_to_be_paid + lr_paid)) %>% 
  select(lr,log_mack_lr,log_mu_prior,lr_paid,lr_to_be_paid,to_be_perc,prior_lr)

### check lognormality assumptions
# old
# mack_est <- MackChainLadder(tri, est.sigma = 'Mack')

# mle_param <- mack_est$FullTriangle %>% apply(2,function(p) fitdistrplus::fitdist(p,distr = 'lnorm',method = 'mle'))
# last_sd <- mle_param[[11]]$estimate[2]


##### I got reference points on the mu parameter space
  

### check variability of log-ultimate cost in data
### look at data to assess a reasonable domain of variation

alpha_start <- data.frame(log_prem = unique(data$premiums) %>% log )
alpha_start$log_ult <- mack_lr %>% select(log_ult_mack = ultimate) %>% log
alpha_start$log_ult_prior <- alpha_start$log_prem + log_priors$log_mu_prior
alpha_start$diff <- alpha_start[,3] - alpha_start[,1]

