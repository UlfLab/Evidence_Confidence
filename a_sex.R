### SEX DIFFERENCES ###############################

### Accuracy ######################################
sex.accuracy<-
  my.data.d %>% 
  group_by(participant,sex) %>%
  summarise(acc_mean=mean(key_resp_direction.corr*100))

ggplot(aes(y=acc_mean,x=sex),data=sex.accuracy) +
  geom_boxplot() +
  ylab("Proportion correct") + xlab("") + theme_classic()+
  geom_hline(yintercept=c(55,70,85),linetype="dashed")

t.test(sex.accuracy$acc_mean~sex.accuracy$sex)


### RT ############################################
sex.rt<-
  my.data.d %>% 
  group_by(participant2,sex)%>%
  summarise(key_resp_direction.rt_mean=mean(key_resp_direction.rt),
            N=n())

ggplot(aes(y=key_resp_direction.rt_mean,x=sex),data=sex.rt) + 
  geom_boxplot() +
  ylab("RT") + xlab("")+theme_classic() + 
  geom_hline(yintercept=0,linetype="dashed")

t.test(sex.rt$key_resp_direction.rt_mean~sex.rt$sex)

sex.rt2<-
  my.data.d %>% 
  group_by(participant,sex,c_choice)%>%
  summarise(key_resp_direction.rt_mean=mean(key_resp_direction.rt)) %>% 
  spread(c_choice,key_resp_direction.rt_mean) %>% 
  mutate(rt_difference=incorrect-correct)

ggplot(aes(y=rt_difference,x=sex),data=sex.rt2) + 
  geom_boxplot() +
  ylab("RT") + xlab("")+theme_classic()  +
  geom_hline(yintercept=0,linetype="dashed")

t.test(sex.rt2$rt_difference~sex.rt2$sex)


### zCONF ############################################
sex.conf<-
  my.data.d %>% 
  group_by(participant,sex,c_choice)%>%
  summarise(zConf_mean=mean(zConf)) %>% 
  spread(c_choice,zConf_mean) %>% 
  mutate(zConf_difference=correct-incorrect)

ggplot(aes(y=zConf_difference,x=sex),data=sex.conf) + 
  geom_boxplot() +
  ylab("RT") + xlab("")+theme_classic()  +
  geom_hline(yintercept=0,linetype="dashed")

t.test(sex.conf2$zConf_difference~sex.conf2$sex)


### BIAS and TIME ###########################################
my.data.a<- 
  my.data.d %>%
  mutate(bias_r=duration_right/(duration_left+duration_right),
         bias_ch=(dwell_chosen)/(dwell_chosen+dwell_unchosen),
         bias=abs((dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen))) %>%
  mutate(time=(duration_left+duration_right)/1000000) %>%
  group_by(participant) %>% 
  mutate(bin_bias_r = ntile(bias_r, 5),
         bin_bias_ch = ntile(bias_ch,5),
         bin_time = ntile(time, 5),
         zBias_r=scale(bias_r),
         zBias_ch=scale(bias_ch),
         zTime = scale(time)) %>% 
  mutate(ch_right=ifelse(key_resp_direction.keys=="right",1,0),
         first_r=ifelse(first_fix=="right",1,0),
         last_r=ifelse(last_fix=="right",1,0))

### BIAS - CHOICE ###########################################
sex_rBias<-
  as.data.frame(my.data.a)%>% 
  group_by(bin_bias_r,participant,sex) %>%
  summarise_at(vars(ch_right,bias_r), funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=ch_right_mean,x=as.factor(bin_bias_r),col=sex),data=sex_rBias)+
  geom_boxplot() +
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\choice_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

#ANOVA
fit <- aov(ch_right_mean ~ sex + bin_bias_r , data=sex_rBias) #+ sex:bin_bias_r
summary(fit)

### BIAS - CONF and ACCURACY ##################################
sex_chBias<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_ch,sex) %>% 
  summarise_at(vars(zConf,bias_ch,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))

## CONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch), col=sex),data=sex_chBias)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\zConf_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

#ANOVA
fit <- aov(zConf_mean ~ sex + bin_bias_ch + sex:bin_bias_ch, data=sex_chBias) #+ sex:bin_bias_r
summary(fit)

## ACCURACY
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_bias_ch), col=sex),data=sex_chBias)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("CHOSEN - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\Accuracy_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

#ANOVA
fit <- aov(key_resp_direction.corr_mean ~ sex + bin_bias_ch + sex:bin_bias_ch, data=sex_chBias) #+ sex:bin_bias_r
summary(fit)

### TIME - CONFIDENCE, ACCURACY ################################
sex_time<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_time,sex) %>% 
  summarise_at(vars(zConf,time,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))

## zCONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_time),col=sex),data=sex_time)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("time") + theme_classic() 
ggsave(filename="!Analysis\\zConf_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

#ANOVA
fit <- aov(zConf_mean ~ sex + bin_time + sex:bin_time, data=sex_time) #+ sex:bin_bias_r
summary(fit)

## ACCURACY
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_time),col=sex),data=sex_time)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("time") + theme_classic() 
ggsave(filename="!Analysis\\Accuracy_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")


#ANOVA
fit <- aov(key_resp_direction.corr_mean ~ sex + bin_time + sex:bin_time, data=sex_time) #+ sex:bin_bias_r
summary(fit)




