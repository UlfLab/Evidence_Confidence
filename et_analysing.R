### WORK ONLY IN THE NONE CONDITION AT THIS POINT

my.data <- my.data.P2

# DOES LAST FIXATION PREDICT THE CHOICE?
  
  my.data$lf.predicts <- ifelse(my.data$key_resp_direction.keys==my.data$last_fix,1,0)

  last.fix<-
    my.data %>%
    select (participant,lf.predicts) %>%
    group_by(participant) %>%
    summarise (MEAN =mean(lf.predicts,na.rm=T))%>%
    summarise (MEAN2 = median(MEAN), sd = sd(MEAN))
  
  # --> overall true in 65% with range (43%-81%)
  # --> 70% (40% sd) new data 
  
# DOES LONGER FIXATION TIME PREDICT CHOICE
  
  my.data$longer.duration<- ifelse(my.data$duration_right>my.data$duration_left,"right","left")
  my.data$longer.dur.pred<- ifelse(my.data$key_resp_direction.keys==my.data$longer.duration,1,0)
  
  longer.dur.pred<-
    my.data %>%
    select (participant,longer.dur.pred) %>%
    group_by(participant) %>%
    summarise (MEAN =mean(longer.dur.pred,na.rm=T))%>%
    summarise( MEAN_P = mean(MEAN), sd=sd(MEAN))
  
  # --> overall true in 64% with range (49%-80%)
  # --> true in 59% (14 sd)  new data

  
  
# DO PEOPLE LOOK LONGER AT CORRECT OPTIONS

# if the right option has longer dwell time how likely it was also chosen/correct
my.data$diff.r_l <- my.data$duration_right-my.data$duration_left

my.data$prob.CH.r <- ifelse(my.data$key_resp_direction.keys=="right",1,0)
my.data$prob.CORR.r <- ifelse(my.data$correct=="right",1,0)

filtered.data<-
  my.data %>%
  filter (changes>1)%>%
  filter(is.na(diff.r_l)!=1)


filtered.data$differences<-cut_number(filtered.data$diff.r_l,12,na.rm=T)
levels(filtered.data$differences)<-seq(1:12)


# difficutl to do
filtered.data$differences<-cut(filtered.data$diff.r_l,9,na.rm=T)
levels(filtered.data$differences)<-seq(1:9)

a <- filtered.data %>%
  group_by(differences)%>%
  summarise(differences2=mean(diff.r_l)/1000)

filtered.data2 <- merge(x=filtered.data,y=a,by="differences")

ch<-
  filtered.data2 %>%
  filter(differences!=1&differences!=12)%>%
  group_by(differences2)%>%
  summarise(prob_corr=mean(prob.CORR.r),prob_choice=mean(prob.CH.r),N=length(cor_longer))%>% #prob_choice=mean(prob.CH.r)
  gather(type,mean,prob_corr,prob_choice)

ggplot(aes(x=as.numeric(differences2),y=mean,col=type),data=ch)+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  xlab("right fixation - left fixation") + 
  ylab( "% right is correct") + 
  ylim(-0.25,0.75)+ 
  scale_fill_manual(values="green")
  


# DOES LONGER FIXATION TIME PREDICT CHOICE - RATIO (RIGHT/(RIGHT+LEFT))
  
# DO PEOPLE LOOK LONGER AT CORRECT CHOICES
my.data$duration_correct <- ifelse(my.data$correct=="right",my.data$duration_right,my.data$duration_left)
my.data$duration_incorrect <- ifelse(my.data$correct=="right",my.data$duration_left,my.data$duration_right)

  
  my.data$cor_longer<- ifelse(my.data$duration_correct>my.data$duration_incorrect,1,0)
  
  cor_longer<-
    my.data %>%
    select (participant,cor_longer,c_choice) %>%
    group_by(participant) %>%
    summarise (MEAN=mean(cor_longer,na.rm=T)) %>%
    summarise (MEAN = mean(MEAN), sd=sd(MEAN))
   
  
  # -> true in 59% cases (48%-67%)
  # -> true in 65% cases if the participant also chose the correct option (48-80%)
  # -> true in 39% cases if the participant chose the incorrect option (17-50%)
  
  # EXCLUDE TRIALS WHERE THERE WAS NOT MUCH DIFFERENCE BETWEEN C/I duration
  my.data$cor_longer.m<- ifelse(abs(my.data$dt_diff)>50000,1,0)
  
  cor_longer.m<-
    my.data %>%
    select (participant,cor_longer,social,c_choice,cor_longer.m) %>%
    filter (social==0, cor_longer.m==1) %>%
    group_by(participant) %>%
    summarise (MEAN=mean(cor_longer,na.rm=T)) #%>%
    #spread (c_choice,MEAN)
  
  # -> 69% in correct cases (52-82%)
  # -> 40% in incorrect cases
  # -> 61% overall

  
  my.data<-my.data.P3
  
# PHASE 3 - BINS AND REALITY
  my.data$duration_correct <- ifelse(my.data$correct=="right",my.data$duration_right,my.data$duration_left)
  my.data$duration_incorrect <- ifelse(my.data$correct=="right",my.data$duration_left,my.data$duration_right)
  
  
  my.data$ratio<-(my.data$duration_correct/(my.data$duration_correct+my.data$duration_incorrect))
  my.data$ratio2<-(my.data$duration_correct/(my.data$weight*1000000))
  my.data$ratio3<-((my.data$weight*1000000-my.data$duration_incorrect)/(my.data$weight*1000000))
  
  
  check<-
    my.data %>% 
    filter(weight==1.25) %>% 
    group_by(strength_prop,participant) %>% 
    summarise(mean=mean(ratio2,na.rm=T),sd=sd(ratio2,na.rm=T),mean2=mean(ratio,na.rm=T),mean3=mean(ratio3,na.rm=T))
  
    ggplot(aes(x=strength_prop,y=mean),data=check) + geom_bar(stat='identity')+ geom_point(aes(y=mean3))+ geom_line(aes(y=strength_prop),linetype="dashed") + facet_wrap(~participant)
  