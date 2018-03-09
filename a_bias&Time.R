### BIAS -  CHOICE, CONFIDENCE AND ACCURACY ################################

### PREPARE DATA FILE #########################################################
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

### CHOICE - BIAS for RIGHT ############################################################

## participant data
bias_right<-
  as.data.frame(my.data.a)%>% 
  group_by(bin_bias_r,participant) %>%
  summarise_at(vars(ch_right,bias_r), funs(mean,sd,sem,ymin,ymax))

#graph - point
ggplot(aes(y=ch_right_mean,x=bias_r_mean),data=bias_right)+
  geom_point(aes(col=participant), show.legend=F)+
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\bias_R_point.jpg", width = 15, height = 15, units = "cm")

#graph - bar/facet
ggplot(aes(y=ch_right_mean,x=bias_r_mean),data=bias_right)+  #as.factor(bin_bias_r)
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=ch_right_ymin,ymax=ch_right_ymax),width=0.1) + 
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic() +
  facet_wrap(~participant) + geom_vline(xintercept=c(0.375,0.5,0.625))
ggsave(filename="!Analysis\\bias_R_facet.jpg", width = 15, height = 15, units = "cm")

## aggregated data  
bias_right_g<-
  bias_right %>% 
  group_by(bin_bias_r) %>% 
  summarise_at(vars(ch_right_mean,bias_r_mean), funs(mean,sd,sem,ymin,ymax))

#graph - bar         
ggplot(aes(y=ch_right_mean_mean,x=bin_bias_r),data=bias_right_g)+ #(round(bias_r_mean_mean,2))
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=ch_right_mean_ymin,ymax=ch_right_mean_ymax),width=0.1) + 
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\bias_R_G_bar.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot
ggplot(aes(y=ch_right_mean,x=as.factor(bin_bias_r)),data=bias_right)+
  geom_boxplot() +
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\bias_R_boxplot.jpg", width = 15, height = 15, units = "cm")


### CONFIDENCE - BIAS FOR CHOSEN ###########################################################
# participant data
bias_chosen<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_ch) %>% 
  summarise_at(vars(zConf,bias_ch,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))  
  
#graph - point
ggplot(aes(y=zConf_mean,x=bias_ch_mean),data=bias_chosen)+
  geom_point(aes(col=participant), show.legend=F)+ facet_wrap(~participant) + geom_vline(xintercept=0.5)+
  geom_smooth(method = "lm")
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\zConf_bias_ch_point.jpg", width = 15, height = 15, units = "cm")

#graph - bar/facet
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch)),data=bias_chosen)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic() +
  facet_wrap(~participant)
ggsave(filename="!Analysis\\zConf_bias_ch_facet.jpg", width = 15, height = 15, units = "cm")

## aggregated data  
bias_chosen_g<-
  bias_chosen %>% 
  group_by(bin_bias_ch) %>% 
  summarise_at(vars(key_resp_direction.corr_mean,zConf_mean,bias_ch_mean),
               funs(mean,sd,sem,ymin,ymax))

#graph - bar         
ggplot(aes(y=zConf_mean_mean,x=as.factor(round(bias_ch_mean_mean,2))),data=bias_chosen_g)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=zConf_mean_ymin,ymax=zConf_mean_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("CHOSEN- sampling bias") + theme_classic()
ggsave(filename="!Analysis\\zConf_bias_ch_G_bar.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch)),data=bias_chosen)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\zConf_bias_ch_G_boxplot.jpg", width = 15, height = 15, units = "cm")


### zConf- correct/incorrect choice #########################################################
bias_chosenCorr<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_ch,key_resp_direction.corr) %>% 
  summarise_at(vars(zConf,bias_ch),funs(mean,sd,sem,ymin,ymax))  

#graph - bar/facet
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch),col=as.factor(key_resp_direction.corr)),data=bias_chosenCorr)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.1) + 
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic() +
  facet_wrap(~participant)

#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch),col=as.factor(key_resp_direction.corr)),data=bias_chosenCorr)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\zConf_bias_ch_G_boxplot.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot facet
ggplot(aes(y=zConf,x=as.factor(bin_bias_ch),col=as.factor(key_resp_direction.corr)),data=my.data.a)+
  geom_boxplot() + facet_wrap(~participant) +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\zConf_bias_ch_G_boxplot_facet.jpg", width = 15, height = 15, units = "cm")

### Accuracy - bias for chosen #########################################################
bias_chosen<-
  as.data.frame(my.data.a) %>%
  group_by(participant,bin_bias_ch) %>% 
  summarise_at(vars(zConf,bias_ch,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))  

#graph - point
ggplot(aes(y=key_resp_direction.corr_mean,x=bias_ch_mean),data=bias_chosen)+
  geom_point(aes(col=participant), show.legend=F)+
  ylab("% correct") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\Corr_bias_ch_point.jpg", width = 15, height = 15, units = "cm")

#graph - bar/facet
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_bias_ch)),data=bias_chosen)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=key_resp_direction.corr_ymin,
                    ymax=key_resp_direction.corr_ymax),width=0.1) + 
  ylab("% correct") + xlab("CHOSEN - sampling bias") + theme_classic() +
  facet_wrap(~participant)
ggsave(filename="!Analysis\\Corr_bias_ch_facet.jpg", width = 15, height = 15, units = "cm")

## aggregated data  
bias_chosen_g<-
  bias_chosen %>% 
  group_by(bin_bias_ch) %>% 
  summarise_at(vars(key_resp_direction.corr_mean,zConf_mean,bias_ch_mean),
               funs(mean,sd,sem,ymin,ymax))

#graph - bar         
ggplot(aes(y=key_resp_direction.corr_mean_mean,x=as.factor(round(bias_ch_mean_mean,2))),data=bias_chosen_g)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=key_resp_direction.corr_mean_ymin,ymax=key_resp_direction.corr_mean_ymax),width=0.1) + 
  ylab("% correct") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\Corr_bias_ch_G_bar.jpg", width = 15, height = 15, units = "cm")

#graph - boxplot
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_bias_ch)),data=bias_chosen)+
  geom_boxplot() +
  ylab("% correct") + xlab("CHOSEN - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\Corr_bias_ch_G_boxplot.jpg", width = 15, height = 15, units = "cm")

####






