###### Utility functions  ########
###### Load in memory before computations ######

#### 1 - function to rearrange parameters ####
###### I rearrange the parameters sets to create the matrices of Mu's and the vector of sigmas ########

mu.sig.df.div.lr <- function(single_set,cdat){
  
  mu.matrix <- function(single_set,cdat){
    
    ### utility function on top 
    
    un_name_list <<- function(vector){
      out <- unname(unlist(vector))
      #slow version
      #out <- vector %>% unlist %>% unname 
      return(out)
    }
    
    ###
    
    t <- length(single_set[[1]])
    
    #Initialize mu
    
    mu <- matrix(0,length(single_set[[1]]),length(single_set[[2]]))
    
    #First Row of mu
    
    mu[1,] <- log_premiums[1] + un_name_list(single_set[[5]][1]) + un_name_list(single_set[[2]])
    
    
    # second row - first part no simulation
    
    mu[2,-t] <- un_name_list(log_premiums[2] + un_name_list(single_set[[5]][2]) + single_set[[1]][2]) + un_name_list(single_set[[2]][1:(t-1)]) + 
      
      + single_set[[4]]*(cdat[cdat$origin == 2,3] - mu[1,-t])
    
    #second row - second part simulation
    
    mu[2,t] <- un_name_list(log_premiums[2] + un_name_list(single_set[[5]][2]) + single_set[[1]][2]) + un_name_list(single_set[[2]][t]) + 
      
      + single_set[[4]]*(log(rlnorm(1,mu[1,t],un_name_list(sqrt(single_set[[3]][t])) )) - mu[1,t])
    
    # third row, cycle to split in part with and without simulation
    
    k <- 2
    
    for(i in 3:t){
      
      # first part - no simulation
      
      mu[i,1:(t-k)] <- un_name_list(log_premiums[i] + un_name_list(single_set[[5]][i]) + single_set[[1]][i]) +
        
        + un_name_list(single_set[[2]][1:(t-k)]) + 
        
        + single_set[[4]]*(cdat[cdat$origin == i,3] - mu[i-1,1:(t-k)])
      
      # second part - simulation
      
      mu[i,(t-k+1):t] <- un_name_list(log_premiums[i] + un_name_list(single_set[[5]][i]) + single_set[[1]][i]) +
        
        + un_name_list(single_set[[2]][(t-k+1):t]) +
        
        single_set[[4]]*(log(rlnorm(k,mu[i-1,(t-k+1):t],un_name_list(sqrt(single_set[[3]][(t-k+1):t])) ) ) - mu[i-1,(t-k+1):t])
      
      k <- k + 1
      
    }
    
    ### rearrange the matrix of mu as a data.frame
    
    colnames(mu) <- 1:length(single_set[[1]])
    rownames(mu) <- 1:length(single_set[[1]])
    
    mu <- as.data.frame(as.triangle(mu)) ### I need the ChainLadder package loaded
    
    mu$origin <-as.numeric(mu$origin)
    
    mu$dev <- as.numeric(mu$dev)
    
    mu$calendar <- mu$origin + mu$dev
    
    colnames(mu) <- c('orig','dev','mu','calendar')
    
    mu <- mu[order(mu$orig),]
    
    rownames(mu) <- 1:dim(mu)[1]
    
    return(mu)
  }
  
  mu <- mu.matrix(single_set,cdat)
  
  ### add a the vector of sigmas
  
  mu$sigma <- rep(single_set[[3]],length(single_set[[2]])) %>% un_name_list
  
  return(mu)
}

#### part related to the one year simulations

#### 2 - functions to simulate first diagonal and likelyhoods ####

low.sim <- function(mu,ndiag){
  mu <- as.data.frame(mu)
  submu <- mu[mu$calendar>(t+1) & mu$calendar < (t+1) + ndiag + 1,]
  rownames(submu) <- 1:dim(submu)[1]
  values <- sapply(1:dim(submu)[1], function(p) rlnorm(1,.subset2(submu,3)[p],sqrt(.subset2(submu,5)[p])))
  submu$values <- values
  submu$lky <- dnorm(log(submu$values),submu$mu,sqrt(submu$sigma))
  submu <- submu[,c(1,4,6,7)]
  return(submu)
} ##Simulate a trapezoid 

### faster alternative to simulate a trapezoid
low.sim.mat <- function(mu,ndiag){
  mu_mat <- as.matrix(mu)
  submu_mat <- mu_mat[mu_mat[,'calendar'] >(t+1) & mu_mat[,'calendar'] < (t+1) + ndiag + 1,]
  submu_mat <- cbind(submu_mat,sqrt(submu_mat[,5]))
  values <- rlnorm(dim(submu_mat)[1], submu_mat[,3],submu_mat[,6])
  lky <- dnorm(log(values),submu_mat[,3],submu_mat[,6])
  submu_mat <- cbind(submu_mat[,c(1,4)],values,lky)
  return(as.data.frame(submu_mat))
}


### compute the likelyhood of each trapezoid
lkyhd <- function(rett,ndiag){
  #rett <- as.data.frame(rett)
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

paid.1yr.batch <- function(simulated,diagonal,discount){
  paid <- sum(simulated[,3] - diagonal) / (1 + discount)
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
    payments[i-lowest_index] = sum(complete[complete$calendar == i,4])/((1+forward[i-lowest_index])**(i-lowest_index))
  }
  return(sum(payments))
}

##### 
sim_recap <- function(vector){
  recap <- c(mean(vector),sd(vector), sd(vector)/mean(vector), skewness(vector),
             kurtosis(vector),quantile(vector,.25),quantile(vector,.5),
              quantile(vector,.75),quantile(vector,.99),quantile(vector,.995) ) 
  names(recap) <- c('Mean','Sd','Vco','Skewness','Kurtosis','1st quartile',
                    'Median','3rd quartile','99% quantile','99.5% quantile')
  options(scipen = 999, digits = 3)
  return(recap)
}

