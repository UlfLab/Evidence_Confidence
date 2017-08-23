

#### STRENGTH SORT INTO GROUPS
nbin = 3

my.data.a<- 
  my.data %>%
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(weight=dwell_correct+dwell_incorrect) %>% 
  mutate(strength_corr=(dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch=(dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen)) %>% 
  mutate(strength_corr2=(dwell_correct)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch2=(dwell_chosen)/(dwell_chosen+dwell_unchosen)) %>% 
#  group_by(participant) %>% 
  mutate(bin_strength_corr = ntile(strength_corr, nbin),bin_weight = ntile(weight, nbin),bin_strength_ch = ntile(strength_ch, nbin))



conf_strength<-
  my.data.a %>% 
  #filter(participant2%in%c(236,663,523)) %>% 
  #filter(participant2%in%c(176,175,745,747)) %>% 
  filter(zConf<3,zConf>-2)%>% 
  group_by(participant,bin_strength_ch) %>% 
  summarise(ZConf=mean(zConf),
            bin_strength_ch2=mean(strength_ch2),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))

ggplot(aes(y=ZConf,x=bin_strength_ch2),data=conf_strength)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(~participant,scales="free")

conf_strength2<-
  conf_strength %>% 
  group_by(participant) %>% 
  mutate(max=bin_strength_ch[ZConf==max(ZConf)],
         min=bin_strength_ch[ZConf==min(ZConf)]) %>% 
  group_by(participant) %>% 
  mutate(conf_type=ifelse(max==2,"R",
                          ifelse(min==2,"V",
                                 ifelse(min==1&max==3,"U","D"))))

ggplot(aes(y=ZConf,x=bin_strength_ch2),data=conf_strength2)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(conf_type~participant,scales="free")

conf_strength3<-
  conf_strength2 %>%
  group_by(conf_type,bin_strength_ch) %>%
  summarise(ZConf=mean(ZConf),
            sd_cor=sd(ZConf),
            N=n(),
            se_cor= sd(ZConf)/ sqrt(n()),
            ymin=mean(ZConf)-sd(ZConf)/ sqrt(n()),
            ymax=mean(ZConf)+sd(ZConf)/ sqrt(n()))

ggplot(aes(y=ZConf,x=bin_strength_ch),data=conf_strength3)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(~conf_type,scales="free")


