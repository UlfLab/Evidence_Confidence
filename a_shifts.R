### Gaze Shift (DWELL TIME - GRATE ON THE RIGHT), SAMPLING BIAS AND ACCURACY ################################

### PREPARE DATA FILE #########################################################
# same data frame as for bias/dTime
my.data.a<- 
  my.data.m %>%
  mutate(bias_r=duration_right/(duration_left+duration_right),
         bias_ch=(dwell_chosen)/(dwell_chosen+dwell_unchosen),
         bias=abs((dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen))) %>%
  group_by(participant) %>% 
  mutate(bin_bias_r = ntile(bias_r, 5),
         bin_bias_ch = ntile(bias_ch,5),
         zBias_r=scale(bias_r),
         zBias_ch=scale(bias_ch)) %>% 
  mutate(ch_right=ifelse(key_resp_direction.keys=="right",1,0),
         first_r=ifelse(first_fix=="right",1,0),
         last_r=ifelse(last_fix=="right",1,0))

### CONFIDENCE - BIAS for RIGHT ############################################################
shift<-
  as.data.frame(my.data.a) %>% 
  mutate(changes_agg=ifelse(changes>6,6,changes)) %>% 
#  filter(changes<7) %>% 
  group_by(participant,changes_agg) %>% 
  summarise_at(vars(corr=key_resp_direction.corr,zConf,bias_ch),funs(mean,sd,sem,ymin,ymax,n=length)) 

#graph - bar/facet
ggplot(aes(y=zConf_mean,x=changes_agg),data=shift)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("Gaze shifts") + theme_classic() +
  geom_text(aes(y = 1.2,label = zConf_n),vjust = 0)+
  facet_wrap(~participant)
ggsave(filename="!Analysis\\zConf_shift_facet.jpg", width = 15, height = 15, units = "cm")

## aggregated data
shift_g<-
  shift%>% 
  group_by(changes_agg) %>% 
  summarise_at(vars(key_resp_direction.corr_mean,zConf_mean,bias_ch_mean),
               funs(mean,sd,sem,ymin,ymax,n=length)) 

#graph - bar         
ggplot(aes(y=zConf_mean_mean,x=as.factor(changes_agg)),data=shift_g)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_mean_ymin,ymax=zConf_mean_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("Gaze shifts") + theme_classic()
ggsave(filename="!Analysis\\zConf_shift_G_bar.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(changes_agg)),data=shift)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("Gaze shifts") + theme_classic() 
ggsave(filename="!Analysis\\zConf_shift_G_boxplot.jpg", width = 15, height = 15, units = "cm")


### ACCURACY - BIAS for RIGHT ############################################################
#graph - bar/facet
ggplot(aes(y=key_resp_direction.corr_mean,x=changes_agg),data=shift)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=key_resp_direction.corr_ymin,ymax=key_resp_direction.corr_ymax),width=0.1) + 
  ylab("% correct") + xlab("Gaze shifts") + theme_classic() +
  geom_text(aes(y = 1.1,label = zConf_n),vjust = 0)+
  facet_wrap(~participant)
ggsave(filename="!Analysis\\Corr_shift_facet.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(changes_agg)),data=shift)+
  geom_boxplot() +
  ylab("% correct") + xlab("Gaze shifts") + theme_classic() 
ggsave(filename="!Analysis\\Corr_shift_G_boxplot.jpg", width = 15, height = 15, units = "cm")

