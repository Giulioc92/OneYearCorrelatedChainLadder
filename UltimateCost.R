library(microbenchmark)
library(tictoc)

########## Simulating the ultimate cost #############
set.seed(19032019)

### triangle dimension variables t x t triangle

t <- cdat %>% select(dev) %>% max

### get the cumulative diagonal

diagonal <- cdat %>% mutate(calendar = dev + origin) %>%
  filter(calendar == max(calendar)) %>% select(logloss) %>% exp %>% unlist %>% unname

#drop oldest value as generation is deemed closed

diagonal <- diagonal[-t] %>% rev


##### Rearrange ALL parameters sets #####
tic()
arranged_sets <- lapply(1:length(sets), function(p) mu.sig.df(sets[[p]],cdat))
toc()

##118 sec room to improve

################ Simulate the ultimate cost

get_ult_res <- function(set){
  
  last_col <- subset(rand_set, calendar > 12 & dev == 11) #[,c(3,5)]
  sim_ultimate <- function(subset_mu,p){
    sigma <- sqrt(.subset2(subset_mu,5)[1])
    mu <- .subset2(last_col,3)[p - 1]
    out <- rlnorm(1,mu, sigma) 
    return(out)
  }
  
  ultimate_costs <- sapply(2:t,function(p) sim_ultimate(last_col,p))
  reserve <- ultimate_costs - diagonal
  #ult_res <- data.frame(ultimate_costs, reserve)
  #return(ult_res)
  return(sum(reserve))
}


ultimate_res <- sapply(sets, function(p) get_ult_res(p))
