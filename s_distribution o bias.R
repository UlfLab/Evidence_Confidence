### Distribution of bias ###########

bin_points<-as.data.frame(quantile(my.data.a$bias_ch,(0:5)/5))
colnames(bin_points)<-"points"


ggplot(aes(x=bias_ch),data=my.data.a)+
  geom_density(alpha=0.2) + geom_vline(aes(xintercept=points),data=bin_points) + #facet_wrap(~participant)+ 
  xlab("CHOSEN - bias") + theme_classic() 
ggsave(filename="!Analysis\\Bias_distribution_boxplot.jpg", width = 15, height = 15, units = "cm")


t<-
  my.data.a%>% 
  filter(last_fix==first_fix) %>% 
  mutate(ff_duration=ifelse(first_fix=="right",duration_right,duration_left),
         lf_duration=ifelse(last_fix=="right",duration_right,duration_left)) %>% 
  group_by(participant) %>% 
  mutate(ff_bias = ff_duration/time,
         lf_bias = lf_duration/time) %>% 
  group_by(participant2) %>% 
  summarise(mean=mean(ff_bias))

t.test(t$mean,mu=0.5)
