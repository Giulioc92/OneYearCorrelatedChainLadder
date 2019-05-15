###### 2 - Inference ######
### need script 1 in memory

### to change the extract in fit_fast

my_seed = 12052019
tic()
########### fit the stan model
ccl_posterior <- stan_model(file = 'ccl_uniform_logpriors.stan')
########### get 10000 posterior sample

fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 5000, init = 'random', iter = 30000,
                thin = 10, chains = 4, cores = 3, control = list(adapt_delta=.999, max_treedepth = 10))

print(fit, pars = c('mu','r_alpha','r_beta', 'a', 'lp__'), include = F)
toc()
print(fit, pars = 'logloss', include = F)

###############
tic()
ccl_posterior <- stan_model(file = 'ccl_uniform_logpriors.stan')
fit_fast <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 2500, init = "random", iter = 7500,
                     thin = 2, chains = 4, cores = 3, control = list(adapt_delta=.99, max_treedepth = 10))
print(fit_fast, pars = c('mu','alpha', 'a', 'lp__'), include = F)
toc()
###############
traceplot(fit, 'alpha')
traceplot(fit, 'beta')
traceplot(fit, 'sig2')
traceplot(fit, 'rho') + scale_color_discrete()
###################
### By looking at the both the R_hat, effective sample size and traceplot 
### Where are confident to have a strong sample, truly representative of
### the posterior distribution
###################

traceplot(fit_fast, 'alpha')
traceplot(fit_fast, 'beta')
traceplot(fit_fast, 'sig2')
traceplot(fit_fast, 'rho') + scale_color_discrete()
traceplot(fit_fast,'logelr11') %>% class
###
extract(fit_fast,'loglossratio[11]') %>% unlist %>% unname %>% exp %>% hist
extract(fit_fast,'loglossratio[10]') %>% unlist %>% unname %>% exp %>% hist
##############  Checks and tests ##################