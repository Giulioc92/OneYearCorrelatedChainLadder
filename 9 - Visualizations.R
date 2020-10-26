#### ccl distribution

cc %>% sim_recap
ccl_BE

cc_df <- as.data.frame(cc)
ccl1YR <- ggplot(cc_df, aes(x = cc_df[,1]))+
  geom_histogram(col = 'blue', fill = 'deepskyblue4', alpha=.6,
                 bins=70)+
  scale_x_continuous('Reserve values') +
  scale_y_continuous('Frequency')+
  ggtitle('One year reserve distribution CCL') +
  theme(plot.title = element_text(face='bold', size=10, hjust = .5),
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10))+
  geom_vline(xintercept = mean(cc), col = 'navy', linetype = 8)+
  geom_vline(xintercept = quantile(cc,.995), col = 'navy', linetype = 8)+
  scale_x_continuous(breaks = c(100000,150000,round(ccl_BE,0),250000,
                                round(max(cc),2)),
                     labels = c(100000,150000,
                                paste('BE(CCL):',round(ccl_BE,0)),paste('99.5%:',259668),
                                round(max(cc),0)),'Reserve Values' )

ccl1YR

cc %>%  sim_recap

##### ultimate view visualization

R = unlist(ultimate_view_reserve)


ccl <- ggplot(as.data.frame(R), aes(x = R))+
  geom_histogram(col = 'red', fill = 'firebrick1', alpha=.6,
                 bins=80)+
  scale_x_continuous('Reserve values') +
  scale_y_continuous('Frequency')+
  ggtitle('Total reserve distribution CCL') +
  theme(plot.title = element_text(face='bold', size=10, hjust = .5),
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10))+
  geom_vline(xintercept = mean(R), col = 'red', linetype = 8) +
  scale_x_continuous(breaks = c(100000,150000,196340,250000,
                                300000,350000),
                     labels = c(100000,150000,
                                paste('E(CCL):',205908),250000,300000,
                                350000), 'Reserve Values')
ccl
ultimate_view_reserve %>% sim_recap

#### bootstrap plot

boot_vec <- unlist(boot$IBNR.Totals)

boot_plot <- ggplot(as.data.frame(boot_vec), aes(x = boot_vec))+
  geom_histogram(col = 'red', fill = 'firebrick1', alpha=.6,
                 bins=70)+
  scale_x_continuous('Reserve values') +
  scale_y_continuous('Frequency')+
  ggtitle('Total reserve distribution') +
  theme(plot.title = element_text(face='bold', size=10, hjust = .5),
        axis.title.x = element_text(face="bold", colour="black", size = 10),
        axis.title.y = element_text(face="bold", colour="black", size = 10)) + 
  geom_vline(xintercept = mean(boot_vec), col = 'red', linetype = 8)

boot_plot


