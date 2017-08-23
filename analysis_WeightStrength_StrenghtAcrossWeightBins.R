### Distribution dwell time and weight
dwell_time<-
  my.data.a %>% 
  gather(corr,dwell,dwell_correct,dwell_incorrect)%>% 
  filter(participant2>115)%>% 
  mutate(b.weight=ntile(weight,5)) %>% 
  group_by(b.weight) %>% 
  mutate(b.weight2=round(mean(weight/1000000),3))

ggplot(aes(x=dwell/1000000,fill=corr),data=dwell_time) + 
  geom_density(alpha=0.2)+ facet_grid(b.weight2~participant2,scales="free")


### dwell time - mean
dwell_time<- 
  my.data %>%
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(weight=dwell_correct+dwell_incorrect) %>% 
  mutate(strength_corr2=(dwell_correct)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch2=(dwell_chosen)/(dwell_chosen+dwell_unchosen)) %>% 
#  group_by(participant) %>% 
  mutate(bin_strength_corr = ntile(strength_corr2, 6),bin_weight = ntile(weight, 3),bin_strength_ch = ntile(strength_ch2, 6)) 
  

dwell_time2<-
  dwell_time %>% 
  group_by(participant,bin_weight) %>% 
  summarise(d_diff = mean((dwell_correct/1000000-dwell_incorrect/1000000),na.rm=T),
            m_strength = mean(strength_corr2))

ggplot(aes(x=bin_weight, y=d_diff),data=dwell_time2)+
  geom_bar(stat="identity")+facet_wrap(~participant,scales="free")
 # geom_hline(yintercept=0)

#### strength_ch/zconf across weight bins
conf_strength<-
  dwell_time %>% 
  filter(zConf<3,zConf>-2)%>% 
  filter(bin_weight==1)%>% 
  group_by(participant,bin_strength_ch,bin_weight) %>% 
  summarise(ZConf=mean(zConf),
            bin_strength_ch2=mean(strength_ch2),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=ZConf,x=bin_strength_ch),data=conf_strength)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(~participant,scales="free")



