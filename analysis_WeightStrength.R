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
nbin = 8

### CALCULATE WEIGHT AND STRENGTH FOR EACH TRIAL #####################
my.data.a<- 
  my.data %>%
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(weight=dwell_correct+dwell_incorrect) %>% 
  mutate(strength_corr=(dwell_correct-dwell_incorrect)/(dwell_correct+dwell_incorrect)) %>% 
  mutate(strength_ch=(dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen)) %>% 
  group_by(participant) %>% 
  mutate(bin_strength_corr = ntile(strength_corr, nbin),bin_weight = ntile(weight, nbin),bin_strength_ch = ntile(strength_ch, nbin))

### SUMMARISE DATA BY BIN AND PLOT IT ################################
corr_strength<-
  my.data.a %>% 
  group_by(participant,bin_strength_corr) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            sd_cor=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
 ggplot(aes(y=mean_cor,x=bin_strength_corr),data=corr_strength)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)

corr_weight<-
  my.data.a %>% 
  group_by(participant,bin_weight) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            sd=sd(key_resp_direction.corr),
            N=n(),
            se_cor= sd(key_resp_direction.corr)/ sqrt(n()),
            ymin=mean(key_resp_direction.corr)-sd(key_resp_direction.corr)/ sqrt(n()),
            ymax=mean(key_resp_direction.corr)+sd(key_resp_direction.corr)/ sqrt(n()))
ggplot(aes(y=mean_cor,x=bin_weight),data=c_a_per_weight)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)

conf_strength<-
  my.data.a %>% 
  group_by(participant,bin_strength_ch) %>% 
  summarise(ZConf=mean(zConf),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=ZConf,x=bin_strength_ch),data=c_a_per_strength)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)

conf_weight<-
  my.data.a %>% 
  group_by(participant,bin_weight) %>% 
  summarise(zConf=mean(zConf),
            sd=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))
ggplot(aes(y=zConf,x=bin_weight),data=c_a_per_weight)+geom_point()+facet_grid(key_resp_direction.corr~participant,scales="free_y")+geom_line()+geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1)






