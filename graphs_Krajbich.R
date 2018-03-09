### MAKE KRAJBICH GRAPHS ##############################################

### FIXATION DURATION ##############################################
my.data.b<-
  my.eye %>% 
  filter(fix%in%c("left","right")) %>% 
  filter(change==1) 

dwell_times <-
  my.data.b %>% 
  group_by(fixPos, participant) %>% 
  summarise(dwellTime=mean(-time))

ggplot(aes(x=fixPos,y=dwellTime),data=dwell_times)+geom_bar(stat="identity")+facet_wrap(~participant)

### LAST FIXATION - TIME ADVANTAGE ###################################
my.data$trialA<-ifelse(my.data$last_fix==my.data$key_resp_direction.keys,1,0)  

last.fix.TA<-
  eye.p %>% 
  filter(fix%in%c("left","right")) %>% 
  filter(change==1)%>% 
  filter(trialA==TRUE)

last.fix.TA.l <-
  eye.p %>% 
  filter(fix%in%c("left","right")) %>% 
  filter(change==1)%>% 
  filter(trialA==TRUE) %>%  
  filter(fixPos=="lastFix")%>% 
  mutate(bin_lastFix=ntile((-time),4))

speeder<-NULL
speeder<-last.fix.TA.l$bin_lastFix
kicker<-rep(0,nrow(last.fix.TA))
for(i in unique(last.fix.TA.l$participant))
  for(j in 1:max(last.fix.TA.l$trial))
    kicker[last.fix.TA$trial==j&last.fix.TA$participant==i]<-speeder[last.fix.TA.l$trial==j&last.fix.TA.l$participant==i]
last.fix.TA$bin_lastFix<-kicker


last.fix.TA$fixAdv<-ifelse(last.fix.TA$fixPos!="lastFix"&last.fix.TA$fix!=last.fix.TA$choice,1,ifelse(last.fix.TA$fixPos=="lastFix",2,0))

last.fix.TA2<-
  last.fix.TA %>% 
  group_by(participant,bin_lastFix,trial) %>% 
  summarise(timeAdv=sum(-time[fixAdv==0]) - sum(-time[fixAdv==1]))%>% 
  group_by(participant,bin_lastFix) %>% 
  summarise(timeAdv2=mean(timeAdv))

ggplot(aes(x=bin_lastFix,y=timeAdv2),data=last.fix.TA2)+geom_bar(stat="identity")+facet_wrap(~participant)

### Strength LEFT - LEFT CHOSEN #####################
my.data.b<-
  my.data %>%
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>%
  mutate(strength_l=(duration_left-duration_right)/(duration_left+duration_right)) %>%
  group_by(participant) %>% 
  mutate(bin_strength_l= ntile(strength_l, 8))

chose.left<-
  my.data.b %>% 
  group_by(participant,bin_strength_l) %>% 
  summarise(chLeft=sum(key_resp_direction.keys=="left"),N=n()) %>% 
  mutate(mean_chLeft=chLeft/N)
ggplot(aes(y=mean_chLeft,x=bin_strength_l),data=chose.left)+geom_point()+facet_wrap(~participant,scales="free_y")+geom_line()


ggplot (aes(x=bin_strength_l,y=))



### TIME ADVANTAGE LEFT - LEFT CHOSEN #####################

my.data.b<-
  my.data %>% 
  mutate(timeAdvL=duration_left-duration_right) %>% 
  filter(!is.na(timeAdvL)) %>%
  mutate(bin_timeAdvL=ntile(timeAdvL,7)) %>% 
  group_by(participant,bin_timeAdvL) %>% 
  summarise(cLeft=sum(key_resp_direction.keys=="left"),n=n()) %>% 
  mutate(pLeft=cLeft/n)

ggplot(aes(x=bin_timeAdvL,y=pLeft),data=my.data.b)+ geom_bar(stat="identity")+facet_wrap(~participant)  

### FIRST DURATION CHOSEN ###############################

firstFix.chosen<-
  eye.p %>% 
  filter(fixPos=="firstFix",change==1) %>% 
  mutate(first_chosen=ifelse(fix==choice,1,0)) %>% 
  mutate(bin_firstFix=ntile(-time,4)) %>% 
  group_by(participant,bin_firstFix) %>% 
  summarise(p_first_chosen=mean(first_chosen))

ggplot(aes(x=bin_firstFix,y=p_first_chosen),data=firstFix.chosen)+ geom_bar(stat="identity") + facet_wrap(~participant)


#### LOOK left, choose left overall ###################

ll<-
  my.data %>% 
  filter(!is.na(key_resp_direction.keys),!is.na(first_fix)) %>% 
  group_by(participant) %>% 
  summarise(Left = sum(key_resp_direction.keys =="left"),ffl = sum(first_fix=="left"),n=n()) %>% 
  mutate(pLeft = Left/n,p_ffl=ffl/n)

ggplot(aes(x=p_ffl,y=pLeft),data=ll)+geom_point()+geom_smooth(method ="lm")