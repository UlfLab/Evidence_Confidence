### CHOICE - BIAS ############################################################

## MODEL
fit.ch <-glmer(choice_l~zBias_l*group_l+(1+zBias_l|partSes_ID),data=my.data.a,family=binomial())

fit.ch.all <-allFit(fit.ch)

fit.ch.mixed <- mixed(choice_l~zBias_l*group_l+(1+zBias_l|partSes_ID),data=my.data.a,family=binomial(),method="LRT")

fit.ch <-glmer(choice_l~zBias_l*group_l+(1+zBias_l|part_ID:session),data=my.data.a,family=binomial(),
               control=glmerControl(optimizer="optimx", optCtrl=list(method = "nlminb")))


plot_model(fit.ch,
           show.p=TRUE,
           show.values=TRUE,
           show.intercept=TRUE,
           vline.color = "black",
           value.offset = 0.3,
           title= "")+theme_bw()

plot_model(fit.ch,
           vline.color = "black",
           type="re")+theme_bw()

confint(fit.ch, method="profile")

summary(fit.ch.all)

ggsave(filename="_Analysis\\Choice_GLM_interactions.jpg", width = 12.5, height = 10, units = "cm")

## GRAPH - BIAS
bias_left<-
  as.data.frame(my.data.a)%>% 
  group_by(bin_bias_l,partSes_ID,group_l) %>%
  summarise_at(vars(ch_left,bias_l), funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=ch_left_mean,x=as.factor(bin_bias_l)),data=bias_left)+
  geom_boxplot()+facet_wrap(~group_l)
  ylab("% LEFT chosen") + xlab("LEFT - sampling bias") + theme_bw() 

ggsave(filename="_Analysis\\choice_L_boxplot.jpg", width = 15, height = 15, units = "cm")

### INDIVIDUAL DIFFERNCES
model.ch<-list()

for(i in 1:length(unique(my.data.a$partSes_ID))){
  
  print(i)
  model.ch[[i]]<-glm(ch_left~zBias_l*group_l, data=subset(my.data.a, partSes_ID == unique(my.data.a$partSes_ID)[i]),family=binomial())
  
}

coef.ch<-list()
for(i in 1:length(unique(my.data.a$partSes_ID))){
  
  coef.ch[[i]]<-
    as.data.frame(summary(model.ch[[i]])$coefficients) %>% 
    mutate(predictor = rownames(.),
           partSes_ID = rep(unique(my.data.a$partSes_ID)[i],length.out=nrow(.)))
}

ch.LM<-rbindlist(coef.ch,use.names=TRUE,fill=TRUE)
colnames(ch.LM)[4]=c("Pr(>|t|)"="pValue")

## GRAPH - BIAs
ch.bias<-
  ch.LM %>% 
  filter(predictor=="zBias_l")

# excluded participants have all ridiculously high estimates
ggplot(aes(x=reorder(as.factor(partSes_ID),Estimate),y=Estimate),data=subset(ch.bias,!partSes_ID%in%c(4714,4733,7463)))+geom_col()


