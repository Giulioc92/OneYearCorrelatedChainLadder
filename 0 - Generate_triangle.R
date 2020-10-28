imported <- read.table("simulated_dataset.txt")

##### simulate other triangles as an example

simulate_triangle <- function(tri){
  
  sim <- sapply(1:dim(tri)[1], function(o) rlnorm(1,mean = tri[o,3],sd = tri[o,4])) %>% as.data.frame
  bind_cols(tri,sim) %>% return()
  
}

set.seed(26)
simulate_triangle(imported) %>% 
  as.triangle(value = ".",dev = 'dev',origin = 'origin') %>%
  MackChainLadder() %>% summary

#### the generated triangle under this seed is exactly the one used for the calculations
