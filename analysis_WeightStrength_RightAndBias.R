### STRENGTH (DWELL TIME - GRATE ON THE RIGHT), SAMPLING BIAS AND ACCURACY ################################

my.data.a<- 
  my.data %>%
  filter(conf>0.1&key_resp_direction.keys!="None") %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(weight=duration_left+duration_right) %>% 
  mutate(strength_r=duration_right/(duration_left+duration_right)) %>% 
  group_by(participant) %>% 
  mutate(bin_strength_r = ntile(strength_r, nbin),bin_weight = ntile(weight, nbin))

### STRENGTH (DWELL TIME - GRATE ON THE RIGHT) ################################
my.data.a$ch_right<-ifelse(my.data.a$key_resp_direction.keys=="right",1,0)

ch_right<-
  my.data.a %>% 
  group_by(participant,bin_strength_r) %>% 
  summarise(chose_right=mean(ch_right))

ggplot(aes(y=chose_right,x=bin_strength_r),data=ch_right)+
  geom_point()+geom_line()+
  facet_wrap(~participant,scales="free")


### SAMPLING BIAS AND ACCURACY ################################
my.data.a$bias<-abs(my.data.a$strength_r-0.5)
my.data.a<-
  my.data.a %>% 
  group_by(participant) %>% 
  mutate(bin_bias = ntile(bias, 5))

corr_bias<-
  my.data.a %>% 
  group_by(participant,bin_bias) %>% 
  summarise(mean_corr=mean((key_resp_direction.corr)),mean_conf=mean(zConf)) #%>% 
  #gather(label,value,mean_corr,mean_conf)

ggplot(aes(y=mean_corr,x=bin_bias),data=corr_bias)+
  geom_point()+geom_line()+
  facet_wrap(~participant,scales="free")
ggplot(aes(y=bias,zConf),data=my.data.a)+geom_point()+stat_smooth(method="lm")+facet_wrap(~participant)
