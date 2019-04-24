### CONFIDENCE - BIAS FOR CHOSEN ###########################################################

fit.conf<-lmer(zConf~zBias_ch*group_ch+time+response.corr+(1|part_ID:session),data=subset(my.data.a))

plot_model(fit.conf,
             show.p=TRUE,
             show.values=TRUE,
             value.offset = 0.4,
             vline.color = "black")+theme_bw()

summary(fit.conf)

ggsave(filename="_Analysis\\GLM_conf.jpg", width = 15, height = 15, units = "cm")

# GRAPH - BIAS
bias_chosen<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_bias_ch,group_ch) %>% 
  summarise_at(vars(zConf,response.corr),list(mean=mean))

ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch)),data=bias_chosen)+
  geom_boxplot() + facet_wrap(~group_ch)+
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_bw()

# GRAPH - TIME
time<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_time,group_ch) %>% 
  summarise_at(vars(zConf,time,response.corr),list(mean=mean))
               
#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_time)),data=time)+
  geom_boxplot() + facet_wrap(~group_ch) +
  ylab("zConfidence") + xlab("sampling time") + theme_bw() 

#### INDIVIDUAL DIFFERENCES ######################################
### CONFIDENCE #############################
model.conf<-list()
plot.conf<-list()

for(i in 1:length(unique(my.data.a$partSes_ID))){
  
  print(i)
  
  model.conf[[i]]<-lm(zConf~zBias_ch*group_ch+zTime,data=subset(my.data.a, partSes_ID == unique(my.data.a$partSes_ID)[i]))
  plot.conf[[i]]<-plot_model(model.conf[[i]],show.values=TRUE,show.P=TRUE,title=paste(unique(my.data.a$partSes_ID)[i]))
  
}

coef<-list()
for(i in 1:length(unique(my.data.a$partSes_ID))){
  
  coef[[i]]<-
    as.data.frame(summary(model.conf[[i]])$coefficients) %>% 
    mutate(predictor = rownames(.),
           participant = rep(unique(my.data.a$partSes_ID)[i],length.out=nrow(.)))
}

conf.LM<-rbindlist(coef,use.names=TRUE,fill=TRUE)
colnames(conf.LM)[4]=c("Pr(>|t|)"="pValue")

# GRAPH - BIAS
conf.bias<-
  conf.LM %>% 
  filter(predictor=="zBias_ch")

ggplot(aes(x=reorder(as.factor(participant),-Estimate),y=Estimate),data=subset(conf.time))+geom_col()

# GRAPH - TIME
conf.time<-
  conf.LM %>% 
  filter(predictor=="zTime")

ggplot(aes(x=reorder(as.factor(participant),-Estimate),y=Estimate),data=subset(conf.time))+geom_col()




