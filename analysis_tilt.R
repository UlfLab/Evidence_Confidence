### TILT ########

corr_tilt<-
  my.data.a %>% 
  mutate(tilt_diff=abs(Rtilt-Ltilt)) %>% 
  mutate(tilt_diff2=ifelse(tilt_diff<180,tilt_diff,tilt_diff-180)) %>% 
  group_by(participant) %>% 
  mutate(bin_tilt = ntile(tilt_diff2,5)) %>% 
  group_by(participant,bin_tilt) %>% 
  summarise(mean_corr=mean(key_resp_direction.corr),
            mean_tilt=mean(tilt_diff2),
            sd_cor=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))

ggplot(aes(y=mean_corr,x=mean_tilt),data=corr_tilt)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(~participant,scales="free")

conf_tilt<-
  my.data.a %>% 
  mutate(tilt_diff=abs(Rtilt-Ltilt)) %>% 
  mutate(tilt_diff2=ifelse(tilt_diff<180,tilt_diff,tilt_diff-180)) %>% 
  group_by(participant) %>% 
  mutate(bin_tilt = ntile(tilt_diff2,5)) %>% 
  group_by(participant,bin_tilt) %>% 
  summarise(mean_conf=mean(zConf),
            mean_tilt=mean(tilt_diff2),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))

ggplot(aes(y=mean_conf,x=mean_tilt),data=conf_tilt)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
  facet_wrap(~participant,scales="free")
