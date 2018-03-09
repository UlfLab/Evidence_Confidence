### SAMPLING TIME - ACCURACY, CONFIDENCE #########################################
my.data.t<- 
  my.data.a %>%
  mutate(time=(duration_left+duration_right)/1000000,
         ch_first =ifelse(first_fix==key_resp_direction.keys,1,0)) %>%
  group_by(participant) %>% 
  mutate(bin_time = ntile(time, 5))
         
### CONFIDENCE - CHOICE ##########################################################
time_choice <-
  as.data.frame(my.data.t) %>%
  group_by(participant,bin_time) %>% 
  summarise_at(vars(ch_first,time),funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=ch_first_mean,x=as.factor(bin_time)),data=time_choice)+
  geom_boxplot() +
  ylab("% option first fixated was chosen") + xlab("sampling time [bins]") + theme_classic()
ggsave(filename="!Analysis\\ChoiceF_time_boxplot.jpg", width = 15, height = 15, units = "cm")

fit <- aov(ch_first_mean ~ bin_time + bin_time, data=time_choice) 
summary(fit)

ggplot(aes(y=ch_first_mean,x=time_mean),data=time_choice)+
  geom_point(aes(col=participant), show.legend=F)+
  ylab("% option first fixated was chosen") + xlab("sampling time [s]") + theme_classic() 

cor.test(time_choice$ch_first_mean,time_choice$time_mean)

fit<-glmer(ch_first~time+(1|participant),data=my.data.t, family = "binomial")
summary(fit)
sjp.glmer(fit, 
          type = "fe", 
          sort = TRUE,
          title = "",
          pred.labels=labels("Sampling_time"))


### CONFIDENCE - TIME ###########################################################
# participant data
time_chosen<-
  as.data.frame(my.data.t) %>%
  group_by(participant,bin_time) %>% 
  summarise_at(vars(zConf,time,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))  

#graph - point
ggplot(aes(y=zConf_mean,x=time_mean),data=time_chosen)+
  geom_point(aes(col=participant), show.legend=F)+
  ylab("zConfidence") + xlab("decision time") + theme_classic()
ggsave(filename="!Analysis\\zConf_time_point.jpg", width = 15, height = 15, units = "cm")

#graph - bar/facet
ggplot(aes(y=zConf_mean,x=as.factor(bin_time)),data=time_chosen)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("time") + theme_classic() +
  facet_wrap(~participant)
ggsave(filename="!Analysis\\zConf_time_facet.jpg", width = 15, height = 15, units = "cm")

# graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_time)),data=time_chosen)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("time") + theme_classic() 
ggsave(filename="!Analysis\\zConf_time_G_boxplot.jpg", width = 15, height = 15, units = "cm")

### ACCURACY - TIME ###########################################################
# participant data
time_chosen<-
  as.data.frame(my.data.t) %>%
  group_by(participant,bin_time) %>% 
  summarise_at(vars(zConf,time,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))  

#graph - point
ggplot(aes(y=key_resp_direction.corr_mean,x=time_mean),data=time_chosen)+
  geom_point(aes(col=participant), show.legend=F)+
  ylab("%correct") + xlab("decision time") + theme_classic()
ggsave(filename="!Analysis\\Corr_time_point.jpg", width = 15, height = 15, units = "cm")

#graph - bar/facet
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_time)),data=time_chosen)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=key_resp_direction.corr_ymin,ymax=key_resp_direction.corr_ymax),width=0.1) + 
  ylab("%correct") + xlab("time") + theme_classic() +
  facet_wrap(~participant)
ggsave(filename="!Analysis\\Corr_time_facet.jpg", width = 15, height = 15, units = "cm")

# graph - boxplot
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_time)),data=time_chosen)+
  geom_boxplot() +
  ylab("%correct") + xlab("bin_time") + theme_classic() 
ggsave(filename="!Analysis\\Corr_time_G_boxplot.jpg", width = 15, height = 15, units = "cm")
