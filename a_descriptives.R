
# BEHAVIOURAL ANALYSIS - Descriptives
############################################################################

# This script outputs the following graphs for each participant:
#   accuracy 
#   mean for correct/incorrect responses - RT, confidence
#   distribution for correct/incorrect responses - RT, confidence


### ACCURACY ##################################################################

s.accuracy<-
  as.data.frame(my.data.a) %>% 
  group_by(partSes_ID) %>%
  summarise_at(vars(response.corr),funs(mean,sem,ymin,ymax))

# mean accuracy
range(s.accuracy$mean)
t.test(s.accuracy$mean)
sem(s.accuracy$mean)


# graph_individual
ggplot(aes(y=mean,x=as.factor(partSes_ID)),data=s.accuracy) +
  geom_bar(stat='identity',position="dodge") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.3)+
  ylab("Proportion correct") + xlab("") + theme_bw()+
  geom_hline(yintercept=0) + geom_hline(yintercept=c(0.70),linetype="dashed")

### MEAN - RT,CONF ##################################################################
s.mean<-
  as.data.frame(my.data.a) %>% 
  group_by(partSes_ID,choice) %>% 
  summarise_at(vars(response.rt,zConf), funs(mean=mean)) 

s.mean.group <-
  s.mean %>% 
  group_by(choice) %>% 
  summarise_at(vars(response.rt_mean,zConf_mean), funs(mean,sem)) 

# zConf
t.test(s.mean$zConf_mean~s.mean$choice,paired=TRUE)
range(s.mean[s.mean$choice=="correct",]$zConf_mean)
range(s.mean[s.mean$choice=="incorrect",]$zConf_mean)

ggplot(aes(y=zConf_mean,x=choice,fill=choice),data=s.mean) + 
  geom_boxplot()+
  ylab("zConf") + xlab("")+theme_bw() +
  geom_hline(yintercept=0,linetype="dashed")

# RT
t.test(s.mean$response.rt_mean~s.mean$choice,paired=TRUE)
range(s.mean[s.mean$choice=="correct",]$response.rt_mean)
range(s.mean[s.mean$choice=="incorrect",]$response.rt_mean)

ggplot(aes(y=response.rt_mean,x=choice,fill=choice),data=s.mean) + 
  geom_boxplot()+
  ylab("Reaction Time") + xlab("")+theme_bw()

mean(s.mean$response.rt_mean)
sem(s.mean$response.rt_mean)

#### Number of fixations per trial #####################
fix.count<-
  my.data.a %>% 
  mutate(nFix5 = ifelse(nFix>5,6,nFix)) %>% 
  group_by(partSes_ID,nFix5) %>%
  summarise(count = n()) %>% 
  group_by(partSes_ID) %>%
  mutate(count_sum = sum(count)) %>% 
  mutate(count_p = count/count_sum)

ggplot(aes(x=as.factor(nFix5),y=count_p),data=fix.count)+geom_boxplot()+ xlab("number of fixations") + ylab("proportion of trials") + theme_bw()

fix.count.group<-
  as.data.frame(fix.count) %>% 
  group_by(nFix5) %>% 
  summarise_at(vars(count_p),funs(mean,sem))

#### Location of first fix #####################
location.first<-
  as.data.frame(my.data.a) %>% 
  group_by(partSes_ID) %>% 
  summarise_at(vars(first_l,first_cor),funs(mean,sem))

t.test(location.first$first_l_mean,mu = 0.5)
range(location.first$first_l_mean)

t.test(location.first$first_cor_mean,mu = 0.5)
range(location.first$first_cor_mean)

#### Choice #############################x
choice.left<-
  as.data.frame(my.data.a) %>% 
  group_by(partSes_ID) %>% 
  summarise_at(vars(choice_l),funs(mean,sem))

t.test(choice.left$mean,mu = 0.5)
range(choice.left$mean)

