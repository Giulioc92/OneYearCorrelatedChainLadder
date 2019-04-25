###### Utility functions for the 1 year simulation ########

#### 1- functions to simulate first diagonal and likelyhoods ####

low.sim <- function(mu,ndiag){
  submu <- mu[mu$calendar>(t+1) & mu$calendar < (t+1) + ndiag + 1,]
  #rownames(submu) <- 1:dim(submu)[1]
  values <- sapply(1:dim(submu)[1], function(p) rlnorm(1,.subset2(submu,3)[p],sqrt(.subset2(submu,5)[p])))
  submu$values <- values
  submu$lky <- dnorm(log(submu$values),submu$mu,sqrt(submu$sigma))
  submu <- submu[,c(1,4,6,7)]
  return(submu)
} ##Simulate a trapezoid 

### compute the likelyhood of each trapezoid
lkyhd <- function(rett,ndiag){
  rett <- as.data.frame(rett)
  lky <- prod(rett[rett$calendar < (t+1) + ndiag +1,4])
  return(lky)
} 

##### from the likelyhood compute the posterior probability of each scenario
posterior_probability <- function(batch,ndiag){
  lky <- sapply(1:length(batch), function(p) lkyhd(batch[[p]],ndiag))
  tutte <- sum(lky)
  paramlky <- lky/tutte
  return(paramlky)
}

######## Compute the next year payments, discounted - For each simulation#######

paid.1yr.batch <- function(simulated,diag,ndiag,discount){
  paid <- sum(simulated[,3] - diagonal) * discount
  return(paid)
}

######## 2 -  expectation of lower triangle ####

lognormal_mean <- function(mu,sigma){
  mean <- exp(mu + (sigma)/2)
  return(mean)
}

expectation_function <- function(mu){
  mu$expectation <- mapply(lognormal_mean, mu[,3], mu[,5])
  return(mu)
}

##### lower triangle of incremental payments #####

get_lower_incremental <- function(average_triangle,t,ndiag = 1){
  complete <- as.triangle(average_triangle,value = 'expectation', dev = 'dev', origin = 'orig')
  incr <- as.data.frame(cum2incr(complete))
  incr$origin <- as.numeric(incr$orig)
  incr$calendar <- incr$origin + incr$dev
  return(incr[incr$calendar > t + ndiag + 1, c(4,2,5,3)])
}

#### get the best estimate implied by a given parameters set
#### function to be fed with a complete triangle of incremental payments

get_nextyear_be <- function(complete,forward){
  payments = NULL
  index <- min(complete$calendar):max(complete$calendar)
  lowest_index <- min(complete$calendar) - 1
  for(i in index){
    payments[i-lowest_index] = sum(complete[complete$calendar == i,4])*forward[i-lowest_index]
  }
  return(sum(payments))
}


