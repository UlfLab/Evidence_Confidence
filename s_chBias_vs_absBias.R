### BIAS CHOSEN VS BIAS ABS ###########################

my.data.a<- 
  my.data.m %>%
  mutate(bias_r=duration_right/(duration_left+duration_right),
         bias_ch=(dwell_chosen)/(dwell_chosen+dwell_unchosen),
         bias=abs((dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen))) %>%
  group_by(participant) %>% 
  mutate(bin_bias_r = ntile(bias_r, 5),
         bin_bias_ch = ntile(bias_ch,5),
         bin_bias_abs = ntile(bias,5),
         zBias_r=scale(bias_r),
         zBias_ch=scale(bias_ch)) %>% 
  mutate(ch_right=ifelse(key_resp_direction.keys=="right",1,0),
         first_r=ifelse(first_fix=="right",1,0),
         last_r=ifelse(last_fix=="right",1,0))

####

bias_abs2<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_abs) %>% 
  summarise_at(vars(zConf,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax)) 


bias_chosen2<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_ch) %>% 
  summarise_at(vars(zConf,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch)),data=bias_chosen2)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.1) + 
  geom_point(aes(y=zConf_mean,x=as.factor(bin_bias_abs)),data=bias_abs2)+
  ylab("zConfidence") + xlab("COMP - sampling bias") + theme_classic() +
  facet_wrap(~participant)



