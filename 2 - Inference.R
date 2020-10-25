###### 2 - Inference ######
### need script 1 in memory

#my_seed_old = 190758
my_seed = 2510
#tic()
########### fit the stan model
ccl_posterior <- stan_model(file = '1b - CCL_log_normalv1.stan')
########### get 10000 posterior sample  ### called here fit_fast for convenienc

# ##### commented code - fast test for running the procedure - not so fast here
# fit <- sampling(ccl_posterior, data = standata,
#                 seed = my_seed,
#                 warmup = 2500,
#                 init = 'random', iter = 27500,
#                 thin = 10, chains = 4, cores = 3,
#                 control = list(adapt_delta=.997, max_treedepth = 12)
#                 )
# ##################
fit
 
fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 2500, init = 'random', iter = 27500,
                thin = 10, chains = 4, cores = 3, control = list(adapt_delta=.998, max_treedepth = 13))

print(fit, pars = c('mu','r_alpha','r_beta', 'a', 'lp__'), include = F)
toc()
print(fit, pars = 'logloss', include = F)

# ##############
# my_seed = 1462019
# tic()
# ccl_posterior <- stan_model(file = 'ccl_lognormal_lossratio6.stan')
# fit <- sampling(ccl_posterior, data = stan_data, seed = my_seed, warmup = 1000, init = "random", iter = 6000,
#                      thin = 2, chains = 1, cores = 3, control = list(adapt_delta=.9, max_treedepth = 12))
# print(fit, pars = c('mu', 'a', 'lp__'), include = F)
# toc()
##############
rstan::traceplot(fit, 'alpha')
rstan::traceplot(fit, 'beta')
rstan::traceplot(fit, 'sig2')
rstan::traceplot(fit, 'rho') + scale_color_discrete()
rstan::traceplot(fit,'elr')
###################
### By looking at the both the R_hat, effective sample size and traceplot 
### Where are confident to have a strong sample, truly representative of
### the posterior distribution
###################

# traceplot(fit_fast, 'alpha')
# traceplot(fit_fast, 'beta')
# traceplot(fit_fast, 'sig2')
# traceplot(fit_fast, 'rho') + scale_color_discrete()
# traceplot(fit_fast,'logelr11') %>% class
# ###
# extract(fit_fast,'loglossratio[11]') %>% unlist %>% unname %>% exp %>% hist
# extract(fit_fast,'loglossratio[10]') %>% unlist %>% unname %>% exp %>% hist
# ##############  Checks and tests ##################
#### Traceplot, Rhat and effective sample size provided in the summaries
#### of the sampling procedure
