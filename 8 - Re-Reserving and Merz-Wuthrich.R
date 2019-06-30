### 8 - Re-Reserving and Merz Wuthrich results

###### Simulating the one year distribution with Re-Reserving approach

#set.seed(1923)
set.seed(9062019)
rere <- data %>% select(origin, dev, cumulative)%>% as.triangle(dev ='dev',
                                                                origin = 'origin', value = 'cumulative') %>%  
  tweedieReserve(rereserving = T, nsim = 10000)
rere$distr.res_1yr %>% sim_recap

on_yr_rere_on_tot <- (rere$distr.res_1yr %>% sim_recap)[3]/ccl_compare[3,2]
###### Getting the Merz-Wuthrich variability
### Recalculate Mack Chain Ladder results
Mack_CL <- data %>% select(origin, dev, cumulative)%>% as.triangle(dev ='dev',
                                                                   origin = 'origin', value = 'cumulative') %>% MackChainLadder(est.sigma = 'Mack')
### Get the claims development results
CDR(Mack_CL)
### Get the total sigma
mw_se <- CDR(Mack_CL)[12,2]
### get the best estimates for the triangle 
### I use a function 
full_triangle <- Mack_CL$FullTriangle %>% cum2incr

#### rfr should be a vector
risk_free <- rfr[,2] %>% unlist %>% unname
best_estimate <- function(full_triangle,risk_free){
  
  #### utilities
  
  discount_function <- function(amount,rate,time){
    d <- amount/(1+rate)**(time-0.5) 
    return(d)
  }
  
  #### triangle or matrix?
  
  check_1 <- ifelse(full_triangle %>% class == c("triangle","matrix"), T, F) %>% sum
  if(check_1 == 2){
    
    full_df <- full_triangle %>% as.data.frame %>% 
      mutate(origin = as.numeric(origin),dev = as.numeric(dev),calendar = origin + dev)
    
    t <- full_df %>% select(origin) %>% max
    max_fut <- t - 1
    dev_start <- full_df %>% select(dev) %>% min
    
    payments <- NULL
    
    for(i in 1:max_fut){
      payments[i] <- full_df %>% filter(calendar == t + i + dev_start) %>% select(value) %>% sum
    }
    
    #### rfr should be a vector
    rates <- risk_free[1:max_fut]
    
    be <- mapply(discount_function,payments,discount,1:(t-1)) %>% sum
    
    return(be)
    
  } else { return(c('please use triangle matrix classes'))}
  
}
be_cl <- best_estimate(full_triangle,risk_free)
mw_cv <- mw_se/be_cl

### I calculate the usp parameter I would obtain choosing Merz and Wuthrich
### and having 11 years of depth in my triangle
sigma_usp <- mw_cv *.74 + .09 * (1-.74)

### I fit a log normal distribution on the be and mw parameter
##### method of moments log-normal ###à
ln_sigma <- function(cv){
  s <- sqrt(log(cv**2 + 1))
  return(s)
}

ln_mu <- function(mean, sig){
  mu <- log(mean) - (sig**2)/2
  return(mu)
}

ln_mw_sig <- ln_sigma(mw_cv)
ln_mw_mu <- ln_mu(be_cl,ln_sigma(mw_cv))
#### check
lognormal_mean(ln_mw_mu,ln_mw_sig**2) ### my function was to be feed with sigma squared

### get vector of quantiles
mapply(qlnorm,c(.75,.99,.995),ln_mw_mu,ln_mw_sig)
### get skewness and kurtosis
lnorm_skew <- function(m,s){
  sk <- (exp(s**2) + 2)*(sqrt(exp(s**2)-1))
  return(sk)
}
lnorm_kurt <- function(s){
  k <- exp(4*s**2) + 2*exp(3*s**2) + 3*exp(2*s**2) - 6
  return(k)
}
lnorm_skew(ln_mw_mu,ln_mw_sig)
lnorm_kurt(ln_mw_sig) + 3

mw_recap <- c(be_cl,mw_se,mw_se/be_cl,lnorm_skew(ln_mw_mu,ln_mw_sig),lnorm_kurt(ln_mw_sig) + 3,
              mapply(qlnorm,c(.25,.5,.75,.99,.995),ln_mw_mu,ln_mw_sig))
names(mw_recap) <- names(rere$distr.res_1yr %>% sim_recap)
mw_recap
mw_oneyr_on_total <- mw_recap[3]/ccl_compare[3,3]
### compare scrs
scr_rere <- ((rere$distr.res_1yr %>% sim_recap)[10] - be_cl) %>% unname
scr_mw_fit <- qlnorm(.995,ln_mw_mu,ln_mw_sig) - be_cl
scr_mw_fit/be_cl
scr_rere/be_cl
