### models_ bias

library(afex)

fit<-mixed(ch_right~zStrength_r+zWeight+factor(first_right)+factor(last_right)+(1|participant),data=my.data.a,family=binomial(), method = "PB")
fit3<-mixed(ch_right~zStrength_r+zWeight+factor(first_right)+factor(last_right)+(1|participant),data=my.data.a,family=binomial(), method = "LRT")


fit5<-glm(ch_right~zStrength_r+zWeight+first_right+last_right,data=filter(my.data.a,participant==3311),family=binomial())
fit6<-glm(zConf~zStrength_r+zWeight+first_right+last_right,data=filter(my.data.a,participant==3311),family=gaussian())



fit2<-glmer(ch_right~zBias_r+zTime+first_r+last_r+(1|participant),data=my.data.a,family=binomial())

library(sjPlot)
set_theme(base = theme_classic(),axis.textsize = 1.7,geom.label.size=9)

sjp.glmer(fit2, 
          type = "fe", 
          sort = TRUE,
          title = "",
          pred.labels=labels("Last_fixation","Sampling_bias","First_fixation","Decision_time"))


fit4<-glmer(factor(key_resp_direction.keys)~zStrength_r+zWeight+factor(first_fix)+factor(last_fix)+(1|participant),data=my.data.a,family=binomial())


my.data.a[,zBias:=scale(as.numeric(bias,na.rm=T)),by=participant]
my.data.a[,zTime:=scale(as.numeric(weight,na.rm=T)),by=participant]
my.data.a[,zBias_ch:=scale(as.numeric(strength_ch2,na.rm=T)),by=participant]
my.data.a[,zStrength_r:=scale(as.numeric(strength_r,na.rm=T)),by=participant]

ggplot(aes(y=zConf,x=bin_strength_ch),data=chose_corr)+
  geom_point()+geom_line()+
  facet_wrap(~participant)

my.data.a$ch_last<-ifelse(my.data.a$last_fix==my.data.a$key_resp_direction.keys,1,0)

fit.corr2<-glmer(key_resp_direction.corr~zBias_ch+zTime+ch_first+ch_last+(1|participant),data=my.data.a,family=binomial())
fit.conf<-lmer(zConf~zBias_ch+zTime+ch_first+ch_last+(1|participant),data=my.data.a)
fit.conf2<-mixed(zConf~zBias_ch+zTime+(1|participant),data=my.data.a)

library(sjPlot)
set_theme(base = theme_classic(),axis.textsize = 1.7,geom.label.size=9)

sjp.glmer(fit.corr2, 
          type = "fe", 
          sort = TRUE,
          title = "",
          pred.labels=labels("Last_fixation","Sampling_bias","First_fixation","Decision_time"))

fit.conf<-lmer(zConf~zBias_ch+zTime+ch_first+ch_last+(1|participant),data=my.data.a)
sjp.lmer(fit.conf, 
         type = "fe", 
         sort = TRUE,
         title = "",
         p.kr = FALSE,
         pred.labels=labels("Last_fixation","Sampling_bias","First_fixation","Decision_time"))




test<-
  my.data.test3 %>% 
  filter(meta>-0.5) %>% 
  group_by(participant) %>% 
  summarise(w_strength=mean(w_strength),
            meta=mean(meta),
            d=mean(d),
            diff=mean(diff))

cor.test(test$w_strength,test$dif)

ggplot(aes(y=w_strength,x=meta),data=test)+
  geom_point()+geom_smooth(method="lm")


corr_bias<-
  my.data.a %>% 
  mutate(bias = abs(strength_l-strength_r)) %>% 
  group_by(participant) %>% 
  mutate(bin_bias=ntile(bias,4)) %>% 
  group_by(participant,bin_bias) %>% 
  summarise(mean_corr=mean((key_resp_direction.corr)),mean_conf=mean(zConf))

fit.corr2<-glmer(key_resp_direction.corr~zBias+zWeight+first_fix_correct+(1|participant),data=my.data.a,family=binomial())

ggplot(filter(my.data.a,participant=1174), aes(x = zStrength_r, y = ch_right)) +
  geom_point(aes(y = predict(fit.corr2, type="response")),size=1) 

ggplot(aes(y=mean_corr,x=bin_bias),data=corr_bias)+
  geom_point()+geom_line()+
  facet_wrap(~participant)

library(arm)

fit.1<-glm(key_resp_direction.corr~zBias*zWeight,data=my.data.a,family=binomial())
fit.1<-glm(ch_right~zStrength_r*zWeight,data=my.data.a,family=binomial())
fit.2<-glm(zConf~zStrength_ch*zWeight,data=my.data.a,family=gaussian())

fit.1<-glmer(key_resp_direction.corr~zBias+zWeight+(1|participant),data=my.data.a,family=binomial())
fit.1<-glmer(ch_right~zStrength_r*zWeight+(1|participant),data=my.data.a,family=binomial())

fit.2<-lmer(zConf~zStrength_ch*zWeight+(zStrength_ch+zWeight|participant),data=my.data.a)

fit.2<-glm(zConf~zStrength_ch+zWeight,data=my.data.a)
display(fit.1)
ranef(fit.2)$participant[2]
