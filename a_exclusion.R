### EXCLUDE TRIALS AND COUNT EXCLUDED TRIALS BY PARTICIPANT #########################

s.exclusion<-exclusion(my.data.m)

p.exclusion<-
  ggplot(aes(y=all,x=as.factor(participant)),data=s.exclusion) +
  geom_bar(stat='identity',position="dodge") + 
  geom_bar(aes(y=resp),stat='identity',fill="red")

s.exclusion.group<-
  s.participant %>% 
# filter(!participant%in%c(7473,9934,4731)) %>% 
  summarise(mean_lost=mean(all),
            sem = sd(all)/sqrt(n()))

### REMOVE EXCLUDED TRIALS FROM MY.DATA #############################################

my.data.e<-
  my.data %>% 
  filter(conf>0.1) %>%
  filter(!is.na(dt_diff))

my.data.se<-
  my.data.s %>% 
  filter(!is.na(dt_diff))%>% 
  filter(!resp.key%in%c("None","[]") & !is.na(resp.key))


### NORMALIZE CONF,RT COLUMNS ###########################################################

my.data.e<-
  my.data.e %>%
  group_by(participant) %>% 
  mutate(zConf = scale(conf),
         zRT = scale(key_resp_direction.rt))

my.data.se<-
  my.data.se %>% 
  mutate(correct=ifelse(resp.key=="right"&resp.corr==1,"right","left"))

save(my.data.se,file='!Data\\phase1_excluded.RData')



# test confidence effect
a<- as.data.frame(abs(my.data.e$zConf-my.data.e$zConf2))
colnames(a)<-c("diff","participant")
a$participant<-my.data.e$participant
ggplot(aes(x=diff),data=a) + geom_histogram() + xlim(0,0.5) + facet_wrap(~participant)



  





