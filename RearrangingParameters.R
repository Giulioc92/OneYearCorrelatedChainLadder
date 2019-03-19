############# Rearrange parameters #################

alpha <- as.data.frame(extract(fit, 'alpha'))
beta <- as.data.frame(extract(fit, 'beta'))
sig2 <- as.data.frame(extract(fit, 'sig2'))
rho <- as.data.frame(extract(fit, 'rho'))
logelr <- as.data.frame(extract(fit, 'logelr'))

colnames(rho) <- 'rho'
colnames(logelr) <- 'logelr'
### logpremiums

log_premiums <- cdat %>% select(logprem) %>% unique %>% unlist %>% unname 

###### I create a list of lists to accomodate for single parameters set #######

### A big list in which every single element is a list of parameters set

param <- list(alpha,beta, sig2, rho,logelr)

sets <- list()
single_set <- list()

for(j in 1:dim(alpha)[1]){
  for(i in 1:length(param)){
    single_set[[i]] <- param[[i]][j,] 
  }
  sets[[j]] <- single_set
}


###### I rearrange the parameters sets to create the matrices of Mu's and the vector of sigmas ########

mu.sig.df <- function(single_set,cdat){
  
  mu.matrix <- function(single_set,cdat){
    
    ### utility function on top 
    
    un_name_list <- function(vector){
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
    
    mu[1,] <- log_premiums[1] + single_set[[5]] + un_name_list(single_set[[2]])
    
    
    # second row - first part no simulation
    
    mu[2,-t] <- un_name_list(log_premiums[2] + single_set[[5]] + single_set[[1]][2]) + un_name_list(single_set[[2]][1:(t-1)]) + 
      
      + single_set[[4]]*(cdat[cdat$origin == 2,3] - mu[1,-t])
    
    #second row - second part simulation
    
    mu[2,t] <- un_name_list(log_premiums[2] + single_set[[5]] + single_set[[1]][2]) + un_name_list(single_set[[2]][t]) + 
      
      + single_set[[4]]*(log(rlnorm(1,mu[1,t],un_name_list(sqrt(single_set[[3]][t])) )) - mu[1,t])
    
    # third row, cycle to split in part with and without simulation
    
    k <- 2
    
    for(i in 3:t){
      
      # first part - no simulation
      
      mu[i,1:(t-k)] <- un_name_list(log_premiums[i] + single_set[[5]] + single_set[[1]][i]) +
        
        + un_name_list(single_set[[2]][1:(t-k)]) + 
        
        + single_set[[4]]*(cdat[cdat$origin == i,3] - mu[i-1,1:(t-k)])
      
      # second part - simulation
      
      mu[i,(t-k+1):t] <- un_name_list(log_premiums[i] + single_set[[5]] + single_set[[1]][i]) +
        
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

