###################################################
### ANALYSE EYE TRACKING DATA - WEIGHT AND STRENGTH
###################################################
# Removes trials where participants released the key prematurely (confidence <0.1)
# Removes trials where participants did not look at both options (dt_diff=NA)
# Calculates strenght of evidence for correct AND chosen options
# Calculates weight of evidence
# Then splits those into n equal-sized bins calculated separately for each participant
# Plots those

### DEFINE N BINS ###################################################

nbin = 5

### CALCULATE WEIGHT AND STRENGTH FOR EACH TRIAL #####################
my.data.a<- 
  my.data %>%
  filter(conf>0.1&key_resp_direction.keys!="None") %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(weight=dwell_correct+dwell_incorrect) %>% 
  mutate(strength_corr=(dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch=(dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen)) %>% 
  mutate(strength_corr2=(dwell_correct)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch2=(dwell_chosen)/(dwell_chosen+dwell_unchosen)) %>% 
  group_by(participant) %>% 
  mutate(bin_strength_corr = ntile(strength_corr2, nbin),bin_weight = ntile(weight, nbin),bin_strength_ch = ntile(strength_ch2, nbin))

### SUMMARISE DATA BY BIN AND PLOT IT ################################

corr_strength<-
  my.data.a %>% 
  group_by(participant,bin_strength_ch) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            bin_strength_ch2= mean(strength_ch2),
            sd_cor=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
 ggplot(aes(y=mean_cor,x=bin_strength_ch2),data=corr_strength)+
   geom_point()+geom_line()+
   geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)+
   facet_wrap(~participant,scales="free")
 
 #
 
corr_weight<-
  my.data.a %>% 
  group_by(participant,bin_weight) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            bin_weight2= mean(weight)/1000000,
            sd=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_weight),data=corr_weight)+
  geom_point()+facet_wrap(~participant,scales="free_y")+
  geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)

cor.test(as.matrix(corr_weight$bin_weight2),as.matrix(corr_weight$mean_cor),method="pearson")

#

conf_strength<-
  my.data.a %>% 
  filter(zConf<3,zConf>-2)%>% 
  group_by(bin_strength_ch) %>% 
  summarise(ZConf=mean(zConf),
            bin_strength_ch2=mean(strength_ch2),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))

ggplot(aes(y=ZConf,x=bin_strength_ch2),data=conf_strength)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.025)
 # facet_wrap(~participant,scales="free")

#

conf_weight<-
  my.data.a %>% 
  group_by(participant,bin_weight) %>% 
  summarise(zConf=mean(zConf),
            bin_weight2=mean(weight/1000000,na.rm=T),
            sd=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=zConf,x=bin_weight2),data=conf_weight)+geom_point()+
  geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.05)+
  facet_wrap(~participant,scales="free")

cor.test(as.matrix(conf_weight$bin_weight2),as.matrix(conf_weight$zConf),method="pearson")

