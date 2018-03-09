### First fix = direction ###################
ff.left<-
  my.data.se %>% 
  filter(contrast>0.1) %>% 
  mutate(first_left=ifelse(first_fix=="left",1,0)) %>% 
  group_by(participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem),ff.left=first_left)

p.ff.left <-
  ggplot(aes(y=ff.left_mean,x=as.factor(participant)),data=ff.left) +
  geom_bar(stat='identity',position="dodge")+geom_hline(yintercept=(c(0.40,0.60)))

t.test(ff.left$ff.left_mean,mu=0.5)

### First fix = correct ###################
ff.correct<-
  my.data.se %>% 
  filter(name=="baseline") %>% 
 # mutate(first_correct=ifelse(first_fix==correct,1,0)) %>% 
  group_by(participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem),ff.correct=first_correct)  


p.ff.choice <-
  ggplot(aes(y=ff.correct_mean,x=as.factor(participant)),data=ff.correct) +
  geom_bar(stat='identity',position="dodge")+geom_hline(yintercept=(c(0.50,0.60)))

t.test(ff.correct$ff.correct_mean,mu=0.5)



### First fix = choice? ###################
ff.choice<-
  my.data.m %>% 
  filter(!is.na(first_fix)) %>% 
  mutate(chose_first=ifelse(first_fix==key_resp_direction.keys,1,0)) %>% 
  group_by(participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem),ff.choice=chose_first)

t.test(ff.choice$ff.choice_mean,mu=0.5)

p.ff.choice <-
  ggplot(aes(y=ff.choice_mean,x=as.factor(participant)),data=ff.choice) +
  geom_bar(stat='identity',position="dodge")

ff.choice.group<-
  ff.choice %>% 
  summarise_each(funs(mean,sd,N=length,sem),ff.choice=ff.choice_mean)

my.data.ff<-merge(my.data.m,select(ff.choice,c(participant,ff.choice_mean)),by="participant",all=TRUE)

my.data.se<-
  my.data.m %>% 
  mutate(ch_first=ifelse(key_resp_direction.keys==first_fix,1,0),
         strength_ff=ifelse(first_fix=="right",duration_right/(duration_left+duration_right),
                            duration_left/(duration_left+duration_right)),
         weight = duration_left+duration_right,
         first_correct=ifelse(first_fix==correct,1,0))
my.data.b<-
  my.data.se %>% 
  filter(name=="baseline")

library(arm)
my.data.se[,zStrength_ff:=scale(as.numeric(strength_ff,na.rm=T)),by=participant]
my.data.se[,zWeight:=scale(as.numeric(weight,na.rm=T)),by=participant]

a.test.e<-list()
a.test.p<-list()
b = 1
my.data.test<-my.data.se
for(i in unique(my.data.se$participant)){
  a = filter(my.data.se,participant==i) 
  a.test.e[[b]]<-coef(summary(glm(ch_first~zStrength_ff*zWeight,data=a,family=binomial())))[,1]
  a.test.p[[b]]<-coef(summary(glm(ch_first~zStrength_ff*zWeight,data=a,family=binomial())))[,4]
  b = b+1
  #d<-as.data.frame(t(coef(a.test[[1]])))
  #d$participant<-i
  #colnames(d)<-c("w_intercept","w_strength","w_weigth","w_interact","participant")
  #e<-rbind(e)
  }

c<-as.data.frame(t(as.data.frame(a.test.e)))
c$participant<-unique(my.data.se$participant)
colnames(c)<-c("w_intercept","w_strength","w_weigth","w_interact","participant")

d<-as.data.frame(t(as.data.frame(a.test.p)))
d$participant<-unique(my.data.se$participant)
colnames(d)<-c("p_intercept","p_strength","p_weigth","p_interact","participant")

f.all <- list(c,d,my.data.test)

# use reduce to call function merge (merges two data.frames)
my.data.test3 <- Reduce(function(...) merge(...,by = "participant", all = TRUE), f.all)

my.data.test4<-
  my.data.test3 %>% 
  group_by(participant) %>% 
  summarise(p_strength=mean(p_strength),
          e_strength=mean(w_strength),
          p_weigth=mean(p_weigth),
          e_weigth=mean(w_weigth))


my.data.test2<-merge(my.data.test,c,by = "participant", all = TRUE)
fit.1<-glmer(ch_first~zStrength_ff*zWeight+(1|participant),data=my.data.se,family=binomial())

m.1<-gam(ch_first~zStrength_ff*zWeight,data=my.data.b,family=binomial())

fit.2<-lmer(zConf~zStrength_ff*zWeight+(1|participant),data=my.data.ff)
fit.2<-glm(zConf~zStrength_ff*zWeight,data=my.data.ff)

my.data.se<-
  my.data.se %>%
  group_by(participant)
  mutate(zContrast=scale(contrast))

fit.1<-glmer(chose_first~contrast+(1|participant),data=my.data.se,family=binomial())
doTest(fit.1, fixed("first_correct", "z"))

my.data.se$first_correct2<-ifelse(my.data.se$first_correct==1,"yes","no")

### first choice bins ############
ch_first<-
  my.data.b %>% 
  group_by(participant) %>% 
  mutate(bin_strength_ff= ntile(strength_ff,4)) %>% 
  group_by(bin_strength_ff,participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem,ymin,ymax),ch_first,strength_ff)

ggplot(aes(y=ch_first_mean,x=strength_ff_mean),data=ch_first)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ch_first_ymin,ymax=ch_first_ymax),width=0.025)+
  facet_wrap(~participant)

ch_first<-
  my.data.ff %>% 
  group_by(participant) %>% 
  mutate(bin_weight= ntile(weight,8)) %>% 
  group_by(bin_weight,participant) %>% 
  summarise_each(funs(mean,sd,N=length,sem,ymin,ymax),ch_first,weight)

ggplot(aes(y=ch_first_mean,x=weight_mean),data=ch_first)+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=ch_first_ymin,ymax=ch_first_ymax),width=0.025)+
  facet_wrap(~participant)
