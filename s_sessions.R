#### Participant and session ID ##################################################

my.data.ses<-
  my.data.a %>% 
  mutate(session=ifelse(substring(participant,4,4)>2,2,1))

a<-unique(my.data.ses[,c('session','participant')])

a<-unique(select(my.data.ses,'session','participant'))
