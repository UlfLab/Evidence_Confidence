### GAM ##########################
library(mgcv)
unique(my.data.a$participant2)
sub.data<-subset(my.data.a,participant2==unique(my.data.a$participant2)[3])
sub.data<-my.data.a
m.1<-gam(zConf~s(weight,strength_ch2),data=sub.data)
summary(m.1)
fit.data<-data.frame(weight=sub.data$weight,strength=sub.data$strength_ch2,fitted=m.1$fitted.values,conf=sub.data$zConf)
vis.gam(m.1,plot.type="contour")
m.2<-gam(key_resp_direction.corr~s(weight,strength_ch2),data=sub.data,family="binomial")
summary(m.2)
fit.data<-data.frame(weight=sub.data$weight,strength=sub.data$strength_ch2,fitted=m.2$fitted.values,value=sub.data$key_resp_direction.corr)
vis.gam(m.2,plot.type="contour")

### DATA FOR HDDM ##########################
my.data.hddm <- 
  my.data.a %>% 
  select(participant,key_resp_direction.corr,key_resp_direction.rt,strength_ch)

colnames(my.data.hddm)=c("participant"="subj_idx", "key_resp_direction.corr"="response","key_resp_direction.rt"="rt","strength_ch"="strength_chosen")
colnames(my.data.hddm)=c("participant"="subj_idx", "key_resp_direction.corr"="response","key_resp_direction.rt"="rt","strength_corr"="strength_correct")

write.csv(my.data.hddm, file = "data_ddm_ch.csv")

### GLM ##########################
sub.data<-subset(my.data.a,participant2==115)
library(arm)
scale(my.data.a$weight)
m.1<-glmer(key_resp_direction.corr~scale(weight)*strength_corr2+(1|participant2),family=binomial,data=my.data.a)
m.1<-glm(sub.data$key_resp_direction.corr~sub.data$weight+I(abs(scale(sub.data$strength_ch2))),family=binomial)
summary(m.1)
plot(my.data.a$zConf~fitted(m.1))
summary(lm(my.data.a$zConf~fitted(m.1)))