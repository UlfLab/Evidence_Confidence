### SEX DIFFERENCES ###############################

### Accuracy ######################################
sex.accuracy<-
  my.data.d %>% 
  group_by(part_ID,sex) %>%
  summarise(acc=mean(response.corr*100))

ggplot(aes(y=acc,x=sex),data=sex.accuracy) +
  geom_boxplot() +
  ylab("Proportion correct") + xlab("") + theme_bw()
  
summary(lm(acc ~ sex,data= sex.accuracy ))

wilcox.test(sex.accuracy$acc_mean~sex.accuracy$sex)


### RT ############################################

summary(lmer(response.rt ~ sex*choice + (1|part_ID:session),data= my.data.d ))

mixed(response.rt ~ sex + (1|part_ID:session),data= my.data.d,method = "LRT")

ggplot (aes(y=response.rt, x=sex),data=my.data.d) + geom_boxplot()


sex.rt<-
  my.data.d %>% 
  group_by(partSes_ID,sex)%>%
  summarise(response.rt_mean=mean(response.rt),
            N=n())

ggplot(aes(y=response.rt_mean,x=sex),data=sex.rt) + 
  geom_boxplot() +
  ylab("RT") + xlab("")+theme_bw() 

wilcox.test(sex.rt$response.rt_mean~sex.rt$sex)

t.test(sex.rt$response.rt_mean~sex.rt$sex)


### zCONF ############################################

summary(lmer(conf ~ sex*choice + (1|part_ID:session),data= my.data.d ))

sex.conf<-
  my.data.d %>% 
  group_by(partSes_ID,sex,choice)%>%
  summarise(zConf_mean=mean(zConf)) 

ggplot(aes(y=zConf_mean,x=sex,fill=choice),data=sex.conf) + 
  geom_boxplot() +
  ylab("zConf") + xlab("")+theme_classic()  +
  geom_hline(yintercept=0,linetype="dashed")


### BIAS - CHOICE ###########################################

fit.ch.s <- glmer(choice_l ~ zBias_l*sex+(1|part_ID:session),data= my.data.d, family=binomial())
fit.ch.s <- glmer(choice_l ~ zBias_l*sex+sex*group_l+(1|part_ID:session),data= my.data.d, family=binomial())


plot_model(fit.ch.s,show.values = TRUE, show.p=TRUE)

sex_lBias<-
  as.data.frame(my.data.d)%>% 
  group_by(bin_bias_l,partSes_ID,sex) %>%
  summarise_at(vars(choice_l,bias_l), funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=choice_l_mean,x=as.factor(bin_bias_l),col=sex),data=sex_lBias)+
  geom_boxplot() + ylab("% LEFT chosen") + xlab("LEFT - sampling bias") + theme_classic() 


### BIAS - CONF and ACCURACY ##################################
fit.acc.s <- glmer(response.corr ~ zBias_ch*group_ch+zBias_ch*sex+zTime*sex+(1|part_ID:session),data= my.data.d, family=binomial())
plot_model(fit.acc.s,show.values = TRUE, show.p=TRUE)

fit.conf.s <- lmer(zConf ~ zBias_ch*sex+zTime*sex+(1|part_ID:session),data= my.data.d)
plot_model(fit.conf.s,show.values = TRUE, show.p=TRUE)

sex_chBias<-
  as.data.frame(my.data.d) %>%
  group_by(partSes_ID,bin_bias_ch,sex) %>% 
  summarise_at(vars(zConf,bias_ch,response.corr),funs(mean,sd,sem,ymin,ymax))

## CONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch), col=sex),data=sex_chBias)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_bw() 
ggsave(filename="!Analysis\\zConf_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

## ACCURACY
ggplot(aes(y=response.corr_mean,x=as.factor(bin_bias_ch), col=sex),data=sex_chBias)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("CHOSEN - sampling bias") + theme_bw() 
ggsave(filename="!Analysis\\Accuracy_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")


### TIME - CONFIDENCE, ACCURACY ################################
sex_time<-
  as.data.frame(my.data.d) %>%
  group_by(partSes_ID,bin_time,sex) %>% 
  summarise_at(vars(zConf,time,response.corr),funs(mean,sd,sem,ymin,ymax))

## zCONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_time),col=sex),data=sex_time)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("time") + theme_bw() 
ggsave(filename="_Analysis\\zConf_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")


## ACCURACY
ggplot(aes(y=response.corr_mean,x=as.factor(bin_time),col=sex),data=sex_time)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("time") + theme_bw() 
ggsave(filename="_Analysis\\Accuracy_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")






