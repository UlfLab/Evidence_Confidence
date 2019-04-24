##### CORRELATIONS BETWEEN MODELS  ####################
acc.first<-
  acc.LM %>% 
  filter(predictor=="zBias_ch:first_ch")

conf.first<-
  acc.LM %>% 
  filter(predictor=="first_ch")


c.first_fix<-
  merge(acc.first,conf.first,by="participant") %>% 
  mutate(Estimate.acc=Estimate.x,
         Estimate.conf=Estimate.y)%>% 
  filter(abs(Estimate.acc)<5) %>% 
  filter(abs(Estimate.conf)<5)

ggplot(aes(x=Estimate.acc,y=Estimate.conf),data=c.first_fix)+geom_point()+xlim(-5,5)+ylim(-5,5)


cor.test(c.first_fix$Estimate.x,c.first_fix$Estimate.y)

