### GLM for every subject #####################################################

my.data.mod<-
  my.data.a %>% 
  group_by(participant) %>% 
  mutate(ch_first = ifelse(key_resp_direction.keys==first_fix,1,0),
         ch_last = ifelse(key_resp_direction.keys==last_fix,1,0),
         zBias_ch = scale(bias_ch),
         zTime = scale(time))

pID<- unique(my.data.a$participant)


#### zCONF #######################################################
zConf.model<-list()
zConf.coef<-list()

for(i in 1:length(pID)){
  
  print(pID[i])
  
  p.data<-
    my.data.mod %>% 
    filter(participant == pID[i])
  
  zConf.model[[i]]<-lm(zConf~zBias_ch+zTime+ch_first+ch_last,data=p.data)
  
  zConf.coef[[i]]<-as.data.frame(summary(zConf.model[[i]])$coefficients)
  zConf.coef[[i]]$label <- rownames(zConf.coef[[i]])
  zConf.coef[[i]]$participant <- rep(pID[i],length.out=nrow(zConf.coef[[i]]))
  }

### Analysis

zConf.LM<-rbindlist(zConf.coef,use.names=TRUE,fill=TRUE) 
colnames(zConf.LM)[4]=c("Pr(>|t|)"="pValue")

zConf.sign<- 
  zConf.LM %>% 
  filter(pValue<0.05) %>% 
  filter(label != "(Intercept)")
  
ggplot(aes(x=label, y=Estimate),data=zConf.sign)+
  geom_bar(stat="identity", position="dodge")+ geom_hline(yintercept=0) +
  facet_wrap(~participant)

### ACCURACY ###########################################

Acc.model<-list()
Acc.coef<-list()

for(i in 1:length(pID)){
  
  print(pID[i])
  
  p.data<-
    my.data.mod %>% 
    filter(participant == pID[i])
  
  Acc.model[[i]]<-glm(key_resp_direction.corr~zBias_ch+zTime+ch_first+ch_last,data=p.data, family ="binomial")
  
  Acc.coef[[i]]<-as.data.frame(summary(Acc.model[[i]])$coefficients)
  Acc.coef[[i]]$label <- rownames(Acc.coef[[i]])
  Acc.coef[[i]]$participant <- rep(pID[i],length.out=nrow(Acc.coef[[i]]))
}

### Analysis

Acc.LM<-rbindlist(Acc.coef,use.names=TRUE,fill=TRUE) 
colnames(Acc.LM)[4]=c("Pr(>|t|)"="pValue")

Acc.sign<- 
  Acc.LM %>% 
  filter(pValue<0.1) %>% 
  filter(label != "(Intercept)")

ggplot(aes(x=label, y=Estimate),data=Acc.sign)+
  geom_bar(stat="identity", position="dodge")+ geom_hline(yintercept=0) +
  geom_point(aes(x=label, y=Estimate),data=zConf.sign)+
  facet_wrap(~participant)


ggplot(aes(x=label, y=Estimate),data=Acc.sign)+
  geom_bar(stat="identity", position="dodge")+ geom_hline(yintercept=0) +
  geom_point(aes(x=label, y=Estimate),data=zConf.sign)+
  
#### 
  zConf.first<-
  zConf.LM %>% 
  filter(label == "ch_first")
  
ch.first<-
  ch.LM %>% 
  filter(label == "first_r")

a<-cor.test(ch.first$Estimate,zConf.first$Estimate)  

plot(zConf.first$Estimate~(ch.first$Estimate))
ch.model[[1]]
ranef

ggplot(aes(xgeom_point()
### CHOICE ###########################################

ch.model<-list()
ch.coef<-list()

for(i in 1:length(pID)){
  
  print(pID[i])
  
  p.data<-
    my.data.mod %>% 
    filter(participant == pID[i])
  
  ch.model[[i]]<-glm(ch_right~zBias_r+first_r+last_r,data=p.data, family ="binomial")
  
  ch.coef[[i]]<-
    as.data.frame(summary(ch.model[[i]])$coefficients) %>% 
    mutate(label = rownames(.),
           participant = rep(pID[i],length.out=nrow(.)))
}

### Analysis

ch.LM<-rbindlist(ch.coef,use.names=TRUE,fill=TRUE) 
colnames(ch.LM)[4]=c("Pr(>|t|)"="pValue")

ch.sign<- 
  ch.LM %>% 
  filter(pValue<0.05) %>% 
  filter(label != "(Intercept)")

ggplot(aes(x=label, y=Estimate),data=ch.sign)+
  geom_bar(stat="identity", position="dodge")+ geom_hline(yintercept=0) +
  facet_wrap(~participant)
