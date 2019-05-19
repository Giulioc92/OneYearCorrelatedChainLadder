###### 2 - Inference ######
### need script 1 in memory

### to change the extract in fit_fast

my_seed = 190758
tic()
########### fit the stan model
ccl_posterior <- stan_model(file = 'ccl_normal_loglossratio_3.stan')
########### get 10000 posterior sample  ### called here fit_fast for convenience

fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 5000, init = 'random', iter = 30000,
                thin = 10, chains = 4, cores = 3, control = list(adapt_delta=.999, max_treedepth = 10))

print(fit, pars = c('mu','r_alpha','r_beta', 'a', 'lp__'), include = F)
toc()
print(fit, pars = 'logloss', include = F)

###############
my_seed = 123456
tic()
ccl_posterior <- stan_model(file = 'ccl_normal_loglossratio_3.stan')
fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 1000, init = "random", iter = 6000,
                     thin = 2, chains = 4, cores = 3, control = list(adapt_delta=.9, max_treedepth = 12))
print(fit, pars = c('mu', 'a', 'lp__'), include = F)
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