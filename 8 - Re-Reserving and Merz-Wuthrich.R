### 8 - Re-Reserving and Merz Wuthrich results

###### Simulating the one year distribution with Re-Reserving approach

#set.seed(1923)
set.seed(9062019)
rere <- data %>% select(origin, dev, cumulative)%>% as.triangle(dev ='dev',
                                                                origin = 'origin', value = 'cumulative') %>%  
  tweedieReserve(rereserving = T, nsim = 10000)
rere$distr.res_1yr %>% sim_recap

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
sigmausp <- mw_cv *.74 + .09 * (1-.74)
sigmausp

