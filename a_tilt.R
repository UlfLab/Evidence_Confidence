### TILT ############################################################

my.data.tilt<-
  as.data.frame(my.data.a) %>% 
  mutate(tilt_diff=abs(Rtilt-Ltilt)) %>% 
  mutate(tilt_diff2=ifelse(tilt_diff<180,tilt_diff,tilt_diff-180),
         tilt_diff3=abs(tilt_diff2-90)) %>% 
  group_by(participant) %>% 
  mutate(bin_tilt = ntile(tilt_diff2,5),
         bin_tilt3 = ntile(tilt_diff3,5)) 

### Accuracy ####
tilt.accuracy <-
  my.data.tilt %>% 
  group_by(participant,bin_tilt) %>% 
  summarise_at(vars(key_resp_direction.corr,zConf,key_resp_direction.rt,tilt_diff2), 
               funs(mean,sd,sem,ymin,ymax,N=length))
  
# boxplot
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_tilt)),data=tilt.accuracy)+
  geom_boxplot()
ggsave(filename="!Analysis\\Accuracy_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

# point
ggplot(aes(y=key_resp_direction.corr_mean,x=tilt_diff2_mean),data=s.tilt)+
  geom_point(aes(col=participant),show.legend = FALSE)


### Confidence ####
tilt.conf <-
  my.data.tilt %>% 
  group_by(participant,bin_tilt,c_choice) %>% 
  summarise_at(vars(key_resp_direction.corr,zConf,key_resp_direction.rt,tilt_diff2), 
               funs(mean,sd,sem,ymin,ymax,N=length))

# boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_tilt),col=c_choice),data=tilt.conf)+ geom_boxplot()
ggsave(filename="!Analysis\\zConf_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

# point - facet
ggplot(aes(y=zConf_mean,x=tilt_diff2_mean, col=c_choice),data=tilt.conf)+
  geom_point()+
  facet_wrap(~participant,scales="free") + 
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.025)
 
### Confidence- difference
tilt.conf2<-
  tilt.conf %>% 
  select(c_choice,zConf_mean,participant,bin_tilt) %>% 
  spread(c_choice,zConf_mean) %>% 
  mutate(zConf_difference=correct-incorrect)

# boxplot
ggplot(aes(y=zConf_difference,x=as.factor(bin_tilt)),data=tilt.conf2) + 
  geom_boxplot() +
  ylab("zConf") + xlab("")+theme_classic()  +
  geom_hline(yintercept=0,linetype="dashed")

fit <- aov(zConf_difference ~ bin_tilt3, data=tilt.conf) #+ sex:bin_bias_r
summary(fit)


### Reaction time ####
tilt.rt <-
  my.data.tilt %>% 
  group_by(participant,bin_tilt,c_choice) %>% 
  summarise_at(vars(key_resp_direction.corr,zConf,key_resp_direction.rt,tilt_diff2), 
               funs(mean,sd,sem,ymin,ymax,N=length))
# boxplot
ggplot(aes(y=key_resp_direction.rt_mean,x=as.factor(bin_tilt),col=c_choice),data=tilt.rt)+ geom_boxplot()
ggsave(filename="!Analysis\\RT_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

# reaction time - correct and incorrect
tilt.rt2<-
  tilt.rt %>% 
  select(key_resp_direction.rt_mean,participant,bin_tilt,c_choice) %>% 
  spread(c_choice,key_resp_direction.rt_mean) %>% 
  mutate(rt_difference=incorrect-correct)

ggplot(aes(y=rt_difference,x=as.factor(bin_tilt)),data=tilt.rt2) + 
  geom_boxplot() +
  ylab("RT") + xlab("")+theme_classic()  +
  geom_hline(yintercept=0,linetype="dashed") 

### Bias and Time 
  



### BIAS AND TIME ####
my.data.a<- 
  my.data.a %>%
  mutate(bias_r=duration_right/(duration_left+duration_right),
         bias_ch=(dwell_chosen)/(dwell_chosen+dwell_unchosen),
         bias=abs((dwell_chosen-dwell_unchosen)/(dwell_chosen+dwell_unchosen))) %>%
  mutate(time=(duration_left+duration_right)/1000000) %>%
  group_by(participant) %>% 
  mutate(bin_bias_r = ntile(bias_r, 5),
         bin_bias_ch = ntile(bias_ch,5),
         bin_time = ntile(time, 5),
         zBias_r=scale(bias_r),
         zBias_ch=scale(bias_ch),
         zTime = scale(time)) %>% 
  mutate(ch_right=ifelse(key_resp_direction.keys=="right",1,0),
         first_r=ifelse(first_fix=="right",1,0),
         last_r=ifelse(last_fix=="right",1,0))


### BIAS AND CHOICE ####
tilt_rBias<-
  as.data.frame(my.data.tilt)%>% 
  group_by(bin_bias_r,participant,bin_tilt) %>%
  summarise_at(vars(ch_right,bias_r), funs(mean,sd,sem,ymin,ymax))

ggplot(aes(y=ch_right_mean,x=as.factor(bin_bias_r),col=as.factor(bin_tilt)),data=tilt_rBias)+
  geom_boxplot() +
  ylab("% RIGHT chosen") + xlab("RIGHT - sampling bias") + theme_classic() 

### BIAS AND CONFIDENCE/ACCURACY ####
tilt_chBias<-
  as.data.frame(my.data.tilt) %>%
  group_by(participant,bin_bias_ch,bin_tilt) %>% 
  summarise_at(vars(zConf,bias_ch,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))

## CONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch), col=as.factor(bin_tilt)),data=tilt_chBias)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("CHOSEN - sampling bias") + theme_classic() 
ggsave(filename="!Analysis\\zConf_Bias_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

## ACCURACY
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_bias_ch), col=as.factor(bin_tilt)),data=tilt_chBias)+
  geom_boxplot() +
  ylab("% correct") + xlab("CHOSEN - sampling bias [bin]") + theme_classic() 


### TIME ####
tilt_time<-
  as.data.frame(my.data.tilt) %>%
  group_by(participant,bin_time,bin_tilt) %>% 
  summarise_at(vars(zConf,time,key_resp_direction.corr),funs(mean,sd,sem,ymin,ymax))

## zCONF
ggplot(aes(y=zConf_mean,x=as.factor(bin_time),col=as.factor(bin_tilt)),data=tilt_time)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("time [bin]") + theme_classic() 
ggsave(filename="!Analysis\\zConf_time_tilt_boxplot.jpg", width = 15, height = 15, units = "cm")

fit <- aov(zConf_mean ~ bin_tilt + bin_time + bin_tilt:bin_time, data=tilt_time) 
summary(fit)

## ACCURACY
ggplot(aes(y=key_resp_direction.corr_mean,x=as.factor(bin_time),col=as.factor(bin_tilt)),data=tilt_time)+
  geom_boxplot() +
  ylab("Accuracy") + xlab("time") + theme_classic() 
ggsave(filename="!Analysis\\Accuracy_time_tilt_boxplot.jpg", width = 10, height = 10, units = "cm")

