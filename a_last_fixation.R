### Last fix = choice? ###################
lf.choice<-
  my.data.e %>% 
  mutate(chose_last=ifelse(last_fix==key_resp_direction.keys,1,0)) %>% 
  group_by(participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem),lf.choice=chose_last)

t.test(lf.choice$lf.choice_mean,mu=0.5)

p.lf.choice <-
  ggplot(aes(y=lf.choice_mean,x=as.factor(participant)),data=lf.choice) +
  geom_bar(stat='identity',position="dodge")

lf.choice.group<-
  lf.choice %>% 
  summarise_each(funs(mean,sd,N=length,sem),lf.choice=lf.choice_mean)

my.data.lf<-merge(my.data.e,select(lf.choice,c(participant,lf.choice_mean)),by="participant",all=TRUE)

### first choice bins ############
ch_last<-
  my.data.lf %>% 
  mutate(ch_last=ifelse(key_resp_direction.keys==last_fix,1,0),
         strength_lf=ifelse(last_fix=="right",duration_right/(duration_left+duration_right),
                            duration_left/(duration_left+duration_right))) %>%
  group_by(participant) %>% 
  mutate(bin_strength_lf= ntile(strength_lf,5)) %>% 
  group_by(bin_strength_lf,participant,lf.choice_mean) %>% 
  summarise_each(funs(mean,sd,N=length,sem),ch_last,strength_lf)

ggplot(aes(y=ch_last_mean,x=strength_lf_mean),data=ch_last)+
  geom_point()+geom_line()+
  facet_wrap(lf.choice_mean~participant)

### Last fixation matches first fixation ###################

ff.lf<-
  my.data.e %>% 
  mutate(firstLast=ifelse(last_fix==first_fix,1,0)) %>% 
  group_by(participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem),ff.lf=firstLast)

p.lf.lf <-
  ggplot(aes(y=ff.lf_mean,x=as.factor(participant)),data=ff.lf) +
  geom_bar(stat='identity',position="dodge")
