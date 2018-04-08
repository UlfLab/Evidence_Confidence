conf.bias.time<-
  my.data.a %>% 
  group_by(participant) %>% 
  mutate(bin_zConf = ntile(zConf,3)) %>% 
  mutate(Hconf = ifelse(bin_zConf==3,1,0),
         Lconf = ifelse(bin_zConf==1,1,0)) %>%
  group_by(bin_time,participant,key_resp_direction.corr) %>% 
  summarise(HC = mean(Hconf),
            LC = mean(Lconf))

 ggplot(aes(x=bin_time, y=HC,fill=as.factor(key_resp_direction.corr)),data=conf.bias.time)+
   geom_bar(stat="identity",position="dodge") + 
   facet_wrap(~participant)
 ggsave(filename="_Analysis\\ConfBias_time_facet.jpg", width = 15, height = 15, units = "cm")
    
 ggplot(aes(x=as.factor(bin_time), y=HC,fill=as.factor(key_resp_direction.corr)),data=conf.bias.time)+
   geom_boxplot()
 ggsave(filename="_Analysis\\ConfBias_time_corr_boxplot.jpg", width = 15, height = 15, units = "cm")
 

 conf.bias.bias<-
   my.data.a %>% 
   group_by(participant) %>% 
   mutate(bin_zConf = ntile(zConf,3)) %>% 
   mutate(Hconf = ifelse(bin_zConf==3,1,0),
          Lconf = ifelse(bin_zConf==1,1,0)) %>%
   group_by(bin_bias_ch,participant) %>% 
   summarise(HC = mean(Hconf),
             LC = mean(Lconf))
  
 ggplot(aes(x=bin_bias_ch, y=HC),data=conf.bias.bias)+geom_bar(stat="identity") + 
   facet_wrap(~participant)
 ggsave(filename="_Analysis\\ConfBias_bias_facet.jpg", width = 15, height = 15, units = "cm")
 
 