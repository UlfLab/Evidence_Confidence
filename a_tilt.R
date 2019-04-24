### TILT ############################################################

my.data.tilt<-
  as.data.frame(my.data.a) %>% 
  mutate(tilt_diff=abs(Rtilt-Ltilt),
         tilt_diff2=ifelse(tilt_diff<180,tilt_diff,tilt_diff-180)) %>% 
  mutate(bin_tilt2 = ntile(tilt_diff2,5),
         zTilt2 = scale(tilt_diff2))

### Accuracy ####

summary(glmer(response.corr~zTilt2+(1|partSes_ID),data=my.data.tilt,family=binomial()))

tilt.accuracy <-
  my.data.tilt %>% 
  group_by(partSes_ID,bin_tilt2) %>% 
  summarise_at(vars(response.corr,zConf,response.rt), 
               funs(mean,sd,sem,ymin,ymax,N=length))
  
# boxplot
ggplot(aes(y=response.corr_mean,x=as.factor(bin_tilt2)),data=tilt.accuracy)+
  geom_boxplot()
ggsave(filename="!Analysis\\Accuracy_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

### RT ############################################

summary(lmer(response.rt ~ zTilt2*choice + (1|part_ID:session),data= my.data.tilt))

ggplot (aes(y=response.rt, x=as.factor(bin_tilt2)),data=my.data.tilt) + geom_boxplot()


tilt.rt<-
  my.data.tilt %>% 
  group_by(partSes_ID,bin_tilt2)%>%
  summarise(response.rt_mean=mean(response.rt),
            N=n())

ggplot(aes(y=response.rt_mean,x=as.factor(bin_tilt2)),data=tilt.rt) + 
  geom_boxplot() +
  ylab("RT") + xlab("tilt - bins")+theme_bw() 

### zCONF ############################################

summary(lmer(conf ~ zTilt2*choice + (1|part_ID:session),data= my.data.tilt ))

summary(lmer(conf ~ as.factor(bin_tilt2) + (1|part_ID:session),data= my.data.tilt ))

summary(lmer(conf ~ zTilt2 + (1|part_ID:session),data= my.data.tilt ))

tilt.conf<-
  my.data.tilt %>% 
  group_by(partSes_ID,bin_tilt2,choice)%>%
  summarise(zConf_mean=mean(zConf)) 

ggplot(aes(y=zConf_mean,x=as.factor(bin_tilt2),fill=choice),data=tilt.conf) + 
  geom_boxplot() +
  ylab("zConf") + xlab("")+theme_bw()

### BIAS - CHOICE ###########################################

fit.ch.tilt <- glmer(choice_l ~ zBias_l*zTilt2+(1|part_ID:session),data= my.data.tilt, family=binomial())
fit.ch.tilt <- glmer(choice_l ~ as.factor(bin_tilt2)+(1|part_ID:session),data= my.data.tilt, family=binomial())


plot_model(fit.ch.tilt,show.values = TRUE, show.p=TRUE,show.intercept = TRUE)

tilt_lBias<-
  as.data.frame(my.data.tilt)%>% 
  group_by(bin_bias_l,partSes_ID,bin_tilt2) %>%
  summarise_at(vars(choice_l,bias_l), funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=choice_l_mean,x=as.factor(bin_tilt2)),data=tilt_lBias)+
  geom_boxplot() + ylab("% LEFT chosen") + xlab("LEFT - sampling bias") + theme_bw() 


### BIAS - CONF and ACCURACY ##################################
fit.acc.tilt <- glmer(response.corr ~ zBias_ch*zTilt2+zTime*zTilt2+(1|part_ID:session),data= my.data.tilt, family=binomial())
fit.acc.tilt <- glmer(response.corr ~ as.factor(bin_bias_ch)*as.factor(bin_tilt2)+zTime+(1|part_ID:session),data= my.data.tilt, family=binomial())

plot_model(fit.acc.tilt,show.values = TRUE, show.p=TRUE)

fit.conf.tilt <- lmer(zConf ~ zBias_ch*zTilt2+zTime*zTilt2+(1|part_ID:session),data= my.data.tilt)
fit.conf.tilt <- lmer(zConf ~ as.factor(bin_bias_ch)*as.factor(bin_tilt2)+zTime+(1|part_ID:session),data= my.data.tilt)
plot_model(fit.conf.tilt,show.values = TRUE, show.p=TRUE)

tilt_chBias<-
  as.data.frame(my.data.tilt) %>%
  group_by(partSes_ID,bin_bias_ch,bin_tilt2) %>% 
  summarise_at(vars(zConf,bias_ch,response.corr),funs(mean,sd,sem,ymin,ymax))

## CONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch), col=as.factor(bin_tilt2)),data=tilt_chBias)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_bw() 
ggsave(filename="_Analysis\\zConf_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")

## ACCURACY
ggplot(aes(y=response.corr_mean,x=as.factor(bin_bias_ch), col=as.factor(bin_tilt2)),data=tilt_chBias)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("CHOSEN - sampling bias") + theme_bw() 
ggsave(filename="_Analysis\\Accuracy_Bias_sex_boxplot.jpg", width = 15, height = 15, units = "cm")


### TIME - CONFIDENCE, ACCURACY ################################
tilt_time<-
  as.data.frame(my.data.tilt) %>%
  group_by(partSes_ID,bin_time,bin_tilt2) %>% 
  summarise_at(vars(zConf,time,response.corr),funs(mean,sd,sem,ymin,ymax))

## zCONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_time),col=as.factor(bin_tilt2)),data=tilt_time)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("time") + theme_bw() 
ggsave(filename="_Analysis\\zConf_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")


## ACCURACY
ggplot(aes(y=response.corr_mean,x=as.factor(bin_time),col=as.factor(bin_tilt2)),data=tilt_time)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("time") + theme_bw() 
ggsave(filename="_Analysis\\Accuracy_time_sex_boxplot.jpg", width = 15, height = 15, units = "cm")






