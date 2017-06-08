
#separate reaction times
data.ddm<-
  my.data %>% 
  select(participant,key_resp_direction.corr,key_resp_direction.rt,strength_conf2)

colnames(data.ddm)<-c("subj_idx","response","rt","strength_chosen")
write.csv(data.ddm,file="ce_data_ddm.csv", row.names = FALSE)


### mystery graph
d<-
  my.data %>% 
  #filter(bin_strength2==1) %>% 
  group_by(participant,bin_weight,bin_strength2) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            mean_conf=mean(zConf))

ggplot(aes(x=bin_weight,y=mean_cor,color=as.factor(bin_strength2)),data=d) + 
  geom_point() + 
  facet_wrap(~participant) + 
  geom_line() #+ 
#  ggtitle(paste("bin_strength =",unique(d$bin_strength2)))





# 

ff.correct<-
  my.data %>% 
  group_by(participant,correct,first_fix) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),
            N=n(),
            mean_conf=mean(zConf))

ggplot(aes(x=correct,y=mean_corr),data=ff.correct)+geom_bar(stat="identity")+facet_grid(first_fix~participant)
ggplot(aes(x=correct,y=mean_conf),data=ff.correct)+geom_bar(stat="identity")+facet_grid(first_fix~participant)
ggplot(aes(x=first_fix,y=N),data=ff.correct)+geom_bar(stat="identity")+facet_wrap(~participant)

ffc.data<-
  my.data %>% 
  group_by(participant,first_fix_correct) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),
            N=n(),
            mean_conf=mean(zConf))

ggplot(aes(x=first_fix_correct,y=mean_corr,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=first_fix_correct,y=mean_conf,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=first_fix_correct,y=N,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)


lr.data<-
  my.data %>% 
  group_by(participant,key_resp_direction.keys) %>% 
  summarise(N=n(),
            mean_corr=mean(key_resp_direction.corr),
            mean_conf=mean(zConf))

ggplot(aes(x=key_resp_direction.keys,y=N),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=key_resp_direction.keys,y=mean_corr),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=key_resp_direction.keys,y=mean_conf),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)

