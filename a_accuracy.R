#### ACCURACY ##########################################x

### MODEL

fit.acc<-glmer(response.corr~zBias_ch*group_ch+zTime+(1+group_ch|part_ID:session),data=subset(my.data.a),family=binomial())

plot_model(fit.acc,
           show.p=TRUE,
           show.values=TRUE,
           show.intercept = TRUE,
           value.offset = 0.4,
           title= "",
           vline.color = "black")+ theme_bw()
summary(fit.acc)

ggsave(filename="_Analysis\\GLM_acc.jpg", width = 15, height = 15, units = "cm")

## GRAPHS - Bias
bias_chosen<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_bias_ch,group_ch) %>% 
  summarise_at(vars(zConf,bias_ch,response.corr),funs(mean,sd,sem,ymin,ymax))  

ggplot(aes(y=response.corr_mean,x=as.factor(bin_bias_ch)),
       data=subset(bias_chosen))+
  geom_boxplot() + facet_wrap(~group_ch,scales="free") + 
  ylab("% correct") + xlab("CHOSEN - sampling bias") + theme_bw() 

ggsave(filename="_Analysis\\Acc_bias_boxplot.jpg", width = 15, height = 15, units = "cm")

## GRAPHS - Time
time<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_time,group_ch) %>% 
  summarise_at(vars(zConf,bias_ch,response.corr,time),list(mean=mean))  

ggplot(aes(y=response.corr_mean,x=as.factor(bin_time)),data=time)+
  geom_boxplot()  + facet_wrap(~group_ch,scales="free") + 
  ylab("% correct") + xlab("Sampling Time") + theme_bw() 

ggsave(filename="_Analysis\\Acc_bias_boxplot.jpg", width = 15, height = 15, units = "cm")


### INDIVIDUAL DATA #####################
model.acc<-list()
plot.acc<-list()


for(i in 1:length(unique(my.data.a$partSes_ID))){
  print(i)
  
  model.acc[[i]]<-glm(response.corr~zBias_ch*group_ch+zBias_cor+zTime, data=subset(my.data.a, partSes_ID == unique(my.data.a$partSes_ID)[i]),family=binomial())
  plot.acc[[i]]<-plot_model(model.acc[[i]],show.values=TRUE,show.P=TRUE,title=paste(unique(my.data.a$partSes_ID)[i]))
}

coef.acc<-list()

for(i in 1:length(unique(my.data.a$partSes_ID))){
  
  coef.acc[[i]]<-
    as.data.frame(summary(model.acc[[i]])$coefficients) %>% 
    mutate(predictor = rownames(.),
           participant = rep(unique(my.data.a$partSes_ID)[i],length.out=nrow(.)))
}

acc.LM<-rbindlist(coef.acc,use.names=TRUE,fill=TRUE)
colnames(acc.LM)[4]=c("Pr(>|t|)"="pValue")

## GRAPH - BIAS
acc.bias<-
  acc.LM %>% 
  filter(predictor=="zBias_ch")

ggplot(aes(x=reorder(as.factor(participant),-Estimate),y=Estimate),data=subset(acc.bias,!participant%in%c(8853,4712,1174)))+geom_col()

## GRAPH - TIME
acc.time<-
  acc.LM %>% 
  filter(predictor=="zTime")

ggplot(aes(x=reorder(as.factor(participant),-Estimate),y=Estimate),data=subset(acc.time))+geom_col()

