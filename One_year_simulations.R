library(parallel)
###### Initialize simulations parameters ######
ndiag <- 1
discount <- 0.99
forward <- seq(.999,.995, length.out = 12)
######

###### 1 - I calculate the future BEST ESTIMATE for each parameters set #######
###### This computation is performed once
###### All these Best Estimates are then weighted according to 
###### the posterior probability distribution obtained from the simulated batch

expectations <- lapply(arranged_sets, function(p) expectation_function(p))

###### from the cumulative expectations I get the lower triangle
###### of incremental payments

lower_incrementals <- lapply(expectations, function(p) get_lower_incremental(p,t,ndiag))

###### I apply the forward discount to obtain the future bests estimates

future_best_estimates <- sapply(lower_incrementals, function(h) get_nextyear_be(h,forward))

###### I have the future best estimates to weigh with the posterior probabilities
###### obtained by the simulations

###### 2- Simulating next year payments development ######

###### I create a function to get one value of the next year obligations #####

next_year_obligations <-function(arranged_sets,diagonal, ndiag, discount, future_best_estimates){
  
  
  ##### Simulation works in batches: for each parameters set I simulate 
  ##### a development scenario. These scenarios are the first batch of simulations
  ##### From a given batch the goal is to obtain a value of the next year obligations
  
  ##### 2.1 - I simulate a batch of 10000 diagonals, 1 for each parameters set
  ##### I have arranged parameters in memory
  
  batch <- lapply(1:length(arranged_sets), function(p) low.sim(arranged_sets[[p]],ndiag))
  
  ##### 2.2 - Get the posterior probability of the simulated batch ######
  
  post_prob <- posterior_probability(batch,1)
  
  ##### 2.3 - Simulate the next year payments #######
  
  paid_oneyear <- sapply(batch, function(p) paid.1yr.batch(p,diagonal,ndiag,discount))
  
  ##### 3 - weigh the best estimates values and obtain a value of the next year obligations
  
  expected_be = weighted.mean(future_best_estimates,post_prob)
  
  next_year_obligations <- sample(paid_oneyear,1) + expected_be
  
  return(next_year_obligations)
  
}

##### I simulate a value #####

tic()
trial_value <- next_year_obligations(arranged_sets,diagonal,1,1.01,future_best_estimates)
toc()

set.seed(25042019)
c1 <- makeCluster(3)
clusterExport(c1, "arranged_sets")
clusterExport(c1, "next_year_obligations")
clusterExport(c1, "low.sim")
clusterExport(c1, "lkyhd")
clusterExport(c1, 'paid.1yr.batch')
clusterExport(c1, 'posterior_probability')
clusterExport(c1, 'diagonal')
clusterExport(c1, 'future_best_estimates')
clusterExport(c1, 't')
tic()
cc <- parSapply(c1,1:1000,function(p) next_year_obligations(arranged_sets,diagonal,1,1.01,future_best_estimates))
toc()
stopCluster(c1)
#
hist(cc, col ='orange')
mean(cc)

