##### 1 - Load packages and load data ##### 
library(rstan)
library(dplyr)
library(ggplot2)
library(ChainLadder)
library(tictoc)
library(profvis)
library(microbenchmark)
library(moments)
library(parallel)
library(readxl)
############# Import the data (a cumulative triangle)
data <- read.table('mtpl_triangle.txt')

############# Manipulation
## log transformation of cumulative and premiums
## flagging with 1 the first accident year

cdat <- data %>% mutate(logloss = log(cumulative),
                        logprem = log(premiums), 
                        origin1id = ifelse(origin == 1,0,1)) %>%
  select(origin,dev,logloss,logprem, origin1id)

## setup a stan datalist for STAN

stan_data <- list(
  len_data <- dim(cdat)[1],
  logprem = cdat$logprem,
  logloss = cdat$logloss,
  origin = cdat$origin,
  dev = cdat$dev,
  l_a = (max(cdat$origin) * (max(cdat$origin)+1))/2,
  origin1id = cdat$origin1id
)

### visualize the list 

stan_data

### 1.1 explore the triangle

tri <- data %>% mutate(value = cumulative) %>% as.triangle(origin = 'origin',dev = 'dev',value = 'value')

t <- cdat %>% select(dev) %>% max
####
plot(tri,lattice = T)
ata(tri)
MackChainLadder(tri, est.sigma = 'Mack')

#### Mack implied loss ratio

mack <- MackChainLadder(tri, est.sigma = 'Mack') %>% summary
mack_lr <- (mack$ByOrigin$Ultimate / unique(data$premiums)) %>% cbind(origin = 1:t) %>%  as.data.frame
colnames(mack_lr)[1] <- 'lr'
#### get some kind of credibility of Mack LR

mack_lr$dev_to_date <- mack$ByOrigin$Dev.To.Date %>%  unlist
### get the diagonal
mack_lr$latest <- mack$ByOrigin$Latest %>% unlist
mack_lr$ultimate <- mack$ByOrigin$Ultimate %>% unlist
mack_lr$loglr <- log(mack_lr$lr)
mack_lr$to_pay <- 1 - mack_lr$dev_to_date
mack_lr$lr_paid <- mack_lr$latest / unique(data$premiums)
mack_lr

  #### benchmark to mix with prior information about loss ratio

#### A uniform distribution is chosen for prior loss ratio
#### I set extremes for loss ratio bands, based on my prior knowledge
#### there could be studied a relationship between the dev_to_date and prior variance

lr_bounds <- matrix(0,nrow = t,ncol = 2)
lr_bounds[2,] <- c(0.7381,0.7382)
lr_bounds[3,] <- c(0.7178,0.7181)
####almost untouched, not real variation on the final loss ratio

lr_bounds[4,] <- c(.728,.732)
lr_bounds[5,] <- c(.77, .79)
lr_bounds[6,] <- c(.825,.87)
lr_bounds[7,] <- c(.66,.73)
lr_bounds[8,] <- c(.75,.80)
lr_bounds[9,] <- c(.66,.75)
lr_bounds[10,] <- c(.67,.75)

### 
###
### 

lr_bounds[11,] <- c(.58,.68)


### the last loss ratio in particular seems not to
### be in line with that ot immediately preceding generations:
### great uncertainty and considerably lower loss ratios
### make us choose a wide prior, centered on higher values than the mack ones

lr_bounds <- lr_bounds %>% as.data.frame()
lr_bounds$origin <- 1:t
colnames(lr_bounds) <- c('lower','upper','origin')


generations_summary <- merge(mack_lr[,1:5], lr_bounds, by = 'origin') 
generations_summary$premiums <- data$premiums %>% unique

options(digits = 2)
generations_summary %>% mutate(d_to_d = dev_to_date,lower_ult = lower*premiums, upper_ult =upper*premiums,
                               'low > lat' = lower*premiums> latest,prior_ult = (lower_ult + upper_ult)/2) %>% 
  select(latest,ultimate,d_to_d, lr,lower,upper,lower_ult,upper_ult, prior_ult,'low > lat')

### log-transform to feed the MCMC sampler

options(digits = 4)
lr_bounds <- lr_bounds %>% mutate(log_low = log(lower),log_up = log(upper))

