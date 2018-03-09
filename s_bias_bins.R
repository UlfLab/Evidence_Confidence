### use fixed bins instead? #######
my.data.fb<- 
  my.data.a %>%
    mutate(bin_bias_ch_f = cut(bias_ch,c(0,0.35,0.45,0.55,0.65,1),labels=c(1,2,3,4,5)),
         bin_bias_r_f = cut(bias_r,c(0,0.35,0.45,0.55,0.65,1),labels=c(1,2,3,4,5)),
         bin_bias_ch_z = ntile(bias_ch,5),
         bin_bias_r_z = ntile(bias_r,5))

bias_right<-
  as.data.frame(my.data.fb)%>% 
  group_by(bin_bias_r_f) %>%
  summarise_at(vars(ch_right), funs(mean,sd,sem,ymin,ymax))


ggplot(aes(y=mean,x=as.factor(bin_bias_r_f)),data=bias_right)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1) + 
  ylab("% RIGHT chosen") + xlab("RIGHT - f.sampling bias") + theme_classic()

bias_chosen<-
  as.data.frame(my.data.fb)%>% 
  group_by(bin_bias_ch_f) %>%
  summarise_at(vars(zConf), funs(mean,sd,sem,ymin,ymax,length))


ggplot(aes(y=mean,x=as.factor(bin_bias_ch_f)),data=bias_chosen)+
  geom_bar(fill="skyblue4",stat="identity")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1) + 
  ylab("% zConf") + xlab("CHOSEN - f.sampling bias") + theme_classic()

bin_bias_ch_f = cut(bias_ch,c(0,0.35,0.45,0.55,0.65,1),labels=c(1,2,3,4))