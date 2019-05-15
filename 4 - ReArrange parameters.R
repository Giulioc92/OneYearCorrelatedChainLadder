############# Rearrange parameters #################

alpha <- as.data.frame(extract(fit_fast, 'alpha'))
beta <- as.data.frame(extract(fit_fast, 'beta'))
sig2 <- as.data.frame(extract(fit_fast, 'sig2'))
rho <- as.data.frame(extract(fit_fast, 'rho'))
logelr <- as.data.frame(extract(fit_fast, 'loglossratio'))



colnames(rho) <- 'rho'
#colnames(logelr) <- 'logelr'
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

#### I create the Mu 

tic()
arranged_sets <- lapply(1:length(sets), function(p) mu.sig.df.div.lr(sets[[p]],cdat))
toc()


