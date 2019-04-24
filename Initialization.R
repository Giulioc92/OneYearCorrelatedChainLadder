library(rstan)
library(dplyr)
library(ChainLadder)
library(tictoc)

############# Import the data
data <- read.table('RF.txt')
data$incurred / data$premium

data %>% mutate(calendar = origin + dev) %>%  filter(calendar == 12) %>% mutate(incurred/premium)
############# Manipulation
## log transformation of cumulative and premiums
## flagging with 1 the first accident year

cdat <- data %>% mutate(logloss = log(cumpaid),
                        logprem = log(premium), 
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

stan_data

tic()
########### fit the stan model
ccl_posterior <- stan_model(file = 'ccl_uniform_logpriors.stan')
########### get 10000 posterior sample
my_seed = 22042019

fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 5000, init = 'random', iter = 30000,
                thin = 10, chains = 4, cores = 3, control = list(adapt_delta=.999, max_treedepth = 10))

print(fit, pars = c('mu','r_alpha','r_beta', 'a', 'lp__'), include = F)
toc()
print(fit, pars = 'logloss', include = F)
#extract(fit, 'loglossratio') %>% as.data.frame %>%  apply(2,mean)
###### more than 10 minutes simulation
# Warning messages:
#   1: There were 127 divergent transitions after warmup. Increasing adapt_delta above 0.9995 may help. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# 2: Examine the pairs() plot to diagnose sampling problems
###############
fit_fast <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 2500, init = "random", iter = 7500,
                     thin = 2, chains = 4, cores = 3, control = list(adapt_delta=.9999, max_treedepth = 15))
print(fit_fast, pars = c('mu','alpha', 'a', 'lp__'), include = F)

############## 2 chains fit
fit_2chains <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 5000, init = "random", iter = 55000,
                        thin = 10, chains = 2, cores = 3, control = list(adapt_delta=.9999, max_treedepth = 15))
print(fit_2chains, pars = c('mu','r_alpha','r_beta', 'a', 'lp__'), include = F)
####### 10 minutes sampling
###############
traceplot(fit, 'alpha')
traceplot(fit, 'beta')
traceplot(fit, 'sig2')
traceplot(fit, 'rho') + scale_color_discrete()
###################
traceplot(fit_2chains, 'alpha')
traceplot(fit_2chains, 'beta')
traceplot(fit_2chains, 'sig2')
traceplot(fit_2chains, 'rho') + scale_color_discrete()
#################
str(fit)

################### other parameters check
