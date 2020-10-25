########## Simulating the ultimate cost #############

### triangle dimension variables t x t triangle

t <- cdat %>% select(dev) %>% max

### get the cumulative diagonal

diagonal <- cdat %>% mutate(calendar = dev + origin) %>%
  filter(calendar == max(calendar)) %>% select(logloss) %>% exp %>% unlist %>% unname
#(despite I already have it)

#drop oldest value as generation is deemed as closed

diagonal <- diagonal[-t] %>% rev

################ Simulate the ultimate cost
get_ult_res <- function(set){
  
  last_col <- subset(set, calendar > 12 & dev == 11) #[,c(3,5)]
  sim_ultimate <- function(subset_mu,p){
    sigma <- sqrt(.subset2(subset_mu,5)[1])
    mu <- .subset2(last_col,3)[p - 1]
    out <- rlnorm(1,mu, sigma) 
    return(out)
  }
  
  ultimate_costs <- sapply(2:t,function(p) sim_ultimate(last_col,p))
  
  reserve <- ultimate_costs - diagonal
  ult_res <- data.frame(ultimate_costs, reserve)
  return(ult_res)
  #return(sum(reserve))
}

ultimate_res <- lapply(arranged_sets, function(p) get_ult_res(p)[,2])

### data.frame with all the generations 

generational_reserves <- as.data.frame(do.call(rbind,ultimate_res))

### generational reserves summaries

generational_recaps <- apply(generational_reserves,2,sim_recap)
colnames(generational_recaps) <- c(2:t)
generational_recaps

#### total reserve

ultimate_view_reserve <- rowSums(generational_reserves)
sim_recap(ultimate_view_reserve)
hist(ultimate_view_reserve, col = 'deepskyblue4')

#### Some other comparisons with Mack's output
#### recalling "mack" from the other script

mack 

mack$ByOrigin[-1,6:4]
gen_rec <- generational_recaps[1:3,] %>% as.matrix %>% t
colnames(gen_rec) <- c('IBNR_CCL','SD_CCL','CV_CCL')

mack_vs_ccl <- cbind(mack$ByOrigin[-1,6:4],gen_rec)[,c(3,4,2,5,1,6)]

mack_vs_ccl

###### ultimate View
mack$Totals[4:6,1]

ccl_compare <- cbind(sim_recap(ultimate_view_reserve),c(mack$Totals[4:6,1], rep(0,7)))
colnames(ccl_compare) <- c('CCL','Mack')

ccl_compare

###### 
set.seed(1892)

boot <- BootChainLadder(tri,10000, process.distr = 'od.pois')
options(digits = 2)

gen_boot <- do.call(rbind,lapply(1:10000, function(p) boot$IBNR.ByOrigin[1:11,1,p]))
#gen_boot

boot_gen_recaps <- apply(gen_boot,2,sim_recap)[,-1]
boot_ccl_recap <- cbind((boot_gen_recaps %>% as.matrix %>% t)[,c(1,2,3,4,9)],
(generational_recaps %>% as.matrix %>% t)[,c(1,2,3,4,9)])[,c(1,6,2,7,3,8,4,9,5,10)]
colnames(boot_ccl_recap) <- c('IBNR_Boot','IBNR_CCL','SD_Boot','SD_CCL','CV_Boot',
                              'CV_CCL','Skew_Boot','Skew_CCL','99%_Boot','99%_CCL')

boot_ccl_recap

##### total reserve comparisons

boot$IBNR.Totals %>% sim_recap

ccl_compare <- cbind(ccl_compare,boot$IBNR.Totals %>% sim_recap)[,c(1,3,2)]
colnames(ccl_compare) <- c('CCL','Bootstrap','Mack')

ccl_compare



#### comparisons again
options(digits = 5)
ccl_compare

#### compare the maximum value of the two simulated distributions
ultimate_view_reserve %>% max
boot$IBNR.Totals %>% max