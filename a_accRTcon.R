
# BEHAVIOURAL ANALYSIS - BASIC GRAPHS
############################################################################

# This script outputs the following graphs for each participant:
#   accuracy 
#   mean for correct/incorrect responses - RT, confidence
#   distribution for correct/incorrect responses - RT, confidence


### CLEAR WORKSPACE ###########################################################

#rm(list=ls())

### PRE-REQ ###################################################################

# Import Packages 
source("f_essentials.R")

# Load my.data (if it is not already present)
if (!exists("my.data.e")){
  load("!Data/data_excluded.RData")
  print("loading my.data")
}

### ACCURACY ##################################################################

s.accuracy<-
  as.data.frame(my.data.a) %>% 
  group_by(participant2) %>%
  summarise(acc_mean=mean(key_resp_direction.corr*100),
            N=length(key_resp_direction.corr),
            cont_mean = mean(contrast/2))

# graph_facet
  ggplot(aes(y=acc_mean,x=as.factor(participant)),data=s.accuracy) +
  geom_bar(stat='identity',position="dodge") +
  ylab("Proportion correct") + xlab("") + theme_classic()+
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(55,70,85),linetype="dashed")
  
  
  ggplot(aes(y=acc_mean,x=cont_mean),data=s.accuracy) +
    geom_point()
  
  fit<-lm(acc_mean~cont_mean,data=s.accuracy)

s.accuracy.group<-
  as.data.frame(s.accuracy) %>% 
  summarise_at(funs(mean,sd,sem,N=length), acc_mean)

### MEAN - RT,CONF ##################################################################
s.mean<-
  as.data.frame(my.data.a) %>% 
  group_by(participant,c_choice) %>% 
  summarise_at(vars(key_resp_direction.rt,zConf), funs(mean,sd,sem,ymin,ymax)) 

ggplot(aes(y=zConf_mean,x=c_choice,fill=c_choice),data=s.mean) + 
  geom_boxplot()+
  ylab("zConf") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")
  # facet_wrap(~participant) +

 t.test(s.mean$zConf_mean~s.mean$c_choice,paired=TRUE)

s.mean.group<-
  as.data.frame(s.mean) %>% 
  group_by(c_choice)%>%
  summarise_at(vars(zConf_mean,key_resp_direction.rt_mean),funs(mean,sd,N=length,sem,ymin,ymax))

# graphs
p.mean.RT <- 
  ggplot(aes(y=RT_mean,x=c_choice,fill=c_choice),data=s.mean.group) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("RT") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
# facet_wrap(~participant) +
  geom_errorbar(aes(ymin=RT_ymin,ymax=RT_ymax),width=0.3)

p.mean.conf <- 
  ggplot(aes(y=zConf_mean,x=c_choice,fill=c_choice),data=s.mean.group) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("zConf") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  # facet_wrap(~participant) +
  geom_errorbar(aes(ymin=zConf_ymin,ymax=zConf_ymax),width=0.3)

# t-test
s.mean.RT.stats<-
  s.mean[,c("participant","c_choice","RT_mean")] %>% 
  spread(c_choice,RT_mean) %>% 
  do(RT= t.test(correct,incorrect,data= .,paired=T))

s.mean.conf.stats<-
  s.mean[,c("participant","c_choice","zConf_mean")] %>% 
  spread(c_choice,zConf_mean) %>% 
  do(RT= t.test(correct,incorrect,data= .,paired=T))

### DISTRIBUTION - RT,CONF ###################################################
p.D.RT <- 
  ggplot(aes(x=key_resp_direction.rt,fill=c_choice),data=my.data.e) + 
  geom_density(alpha=0.2) + facet_wrap(~participant)

p.D.conf <- 
  ggplot(aes(x=zConf,fill=c_choice),data=my.data.e) + 
  geom_density(alpha=0.2) + facet_wrap(~participant)

p.D.d <- 
  ggplot(aes(y=d,x=1),data=my.data.m) + 
  geom_boxplot()

