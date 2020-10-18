############# Rearrange parameters #################

alpha <- as.data.frame(rstan::extract(fit, 'alpha'))
beta <- as.data.frame(rstan::extract(fit, 'beta'))
sig2 <- as.data.frame(rstan::extract(fit, 'sig2'))
rho <- as.data.frame(rstan::extract(fit, 'rho'))
logelr <- as.data.frame(rstan::extract(fit, 'loglossratio'))
rho <- as.data.frame(rstan::extract(fit, 'rho'))


colnames(rho) <- 'rho'
### plot the posterior distribution of rho
ggplot(rho, aes(x=rho))+
  geom_histogram(bins= 60, color = 'darkcyan', fill = 'cyan4',
                 alpha = .7) + 
  scale_x_continuous(expression(rho)) +
  scale_y_continuous('Frequency')+
  ggtitle(expression(paste(rho,' - ' ,'Posterior Distribution'))) +
  theme(plot.title = element_text(face='bold', size=10, hjust = .5),
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(colour="black", size = 10)) +
  geom_vline(xintercept = mean(rho %>% unlist), col = 'deepskyblue4', linetype = 8)
#### add a dashed line
rho %>% unlist %>% sim_recap


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
set.seed(2244)

tic()
arranged_sets <- lapply(1:length(sets), function(p) mu.sig.df.div.lr(sets[[p]],cdat))
toc()

 
####### caterpillar plots
detach('package:rstan', unload = T)
library(coda)

alpha_mcmc <- as.mcmc(alpha)
beta_mcmc <- as.mcmc(beta)
sigma_mcmc <- as.mcmc(sig2)
elr_mcmc <- as.mcmc(exp(logelr))
colnames(elr_mcmc) <- sapply(1:11,function(p) paste('elr',p,sep = ""))

aa <- alpha_mcmc %>% ggs() %>% ggs_caterpillar()
bb <- beta_mcmc %>% ggs() %>% ggs_caterpillar()
ss <- sigma_mcmc %>% ggs() %>% ggs_caterpillar()
ee <- elr_mcmc %>% ggs() %>% ggs_caterpillar()


grid.arrange(grid.arrange(aa,ss,nrow=1),grid.arrange(bb,ee,nrow = 1), nrow = 2)

#detach("package:coda",unload = T)

########## put here some code to extract the samples
########## in order not to have to run the mcmc procedure all the time

bind_cols(alpha,beta,sig2,rho,logelr) %>% write.csv("samples.csv")
save(fit, file = "stan_model_final.rda")

