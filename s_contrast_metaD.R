#### META - contrast #######################
meta.contrast<-
  as.data.frame(my.data.a) %>% 
  group_by(participant) %>% 
  summarise_at(vars(contrast,key_resp_direction.corr,meta,diff),funs(mean,sem,ymin,ymax))

ggplot(aes(x=contrast_mean,y=diff_mean, col=participant),data=meta.contrast)+
  geom_point()+geom_smooth(method="lm")

cor.test(meta.contrast$key_resp_direction.corr_mean,meta.contrast$meta_mean)
cor.test(meta.contrast$contrast_mean,meta.contrast$diff_mean)

fit<-lm(meta_mean~contrast_mean+key_resp_direction.corr_mean,data=meta.contrast)
