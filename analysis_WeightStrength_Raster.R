#### Raster plot weight vs strength
library(gridExtra)

wStrenght<-
  my.data.a %>% 
  filter(zConf<3,zConf>-2)%>% 
  group_by(participant,bin_weight,bin_strength_ch) %>% 
  summarise(ZConf=mean(zConf),
            mean_cor=mean(key_resp_direction.corr),
            bin_strength_ch2=mean(strength_ch2),
            sd_cor=sd(zConf),
            N=n())

p.conf<-ggplot(aes(y=bin_weight,x=bin_strength_ch),data=wStrenght)+
  geom_raster(aes(fill=ZConf))

f.conf <- group_by(wStrenght,participant) %>%
  do(ACC = p.conf %+% ., 
     ACC_F = p.conf+ facet_wrap(~participant,scales="free"))

p.corr<-ggplot(aes(y=bin_weight,x=bin_strength_ch),data=wStrenght)+
  geom_raster(aes(fill=mean_cor))
facet_wrap(~participant,scales="free")

f.corr <- group_by(wStrenght,participant) %>%
  do(ACC = p.corr %+% ., 
     ACC_F = p.corr+ facet_wrap(~participant,scales="free"))

f.combined<-list()

part = unique(my.data.a$participant)
for(i in 1:length(part)){
  f.combined[[i]]<-grid.arrange(f.conf$ACC[[i]], f.corr$ACC[[i]],ncol = 2, nrow = 1,top = part[i])
}


plot(f.combined[[3]])
