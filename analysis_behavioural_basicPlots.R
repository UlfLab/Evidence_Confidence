################################################################
# BEHAVIOURAL ANALYSIS - BASIC GRAPHS
################################################################

# This script outputs the following graphs for each participant:
#   accuracy 
#   mean RT for correct/incorrect responses
#   distribution of RT times for correct/incorrect responses
#   mean confidence for correct/incorrect responses
#   distribution of confidence for correct/incorrect responses

# Outputs a data frame for each condition
# AND one compiled data frame with all graphs

### CLEAR WORKSPACE ###########################################################

#rm(list=ls())

### PRE-REQ ###################################################################

# Import Packages 
source("functions_essentials.R")

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("!Data/ce_behavioural.RData")
  print("loading my.data")
}

### NUMBER OF TRIALS ##################################################################

s.trials<-
  my.data %>% 
  filter(conf>0.1) %>%
  filter(!is.na(dt_diff)) %>%
  group_by(participant) %>%
  summarise(N=n())

p.trials<-
  ggplot(aes(y=N,x=as.factor(participant)),data=s.trials) +
  geom_bar(stat='identity',position="dodge")


### ACCURACY ##################################################################

s.accuracy<-
  my.data %>% 
  filter(conf>0.1) %>%
  group_by(participant) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),N=length(key_resp_direction.corr))

p.accuracy <-
  ggplot(aes(y=mean_corr,x=as.factor(participant)),data=s.accuracy) +
  geom_bar(stat='identity',position="dodge") +
  ylab("Proportion correct") + xlab("") + theme_classic()+
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=70,linetype="dashed") + 
  geom_hline(yintercept=82.5,linetype="dashed") + 
  geom_hline(yintercept=57.5,linetype="dashed") 

f.accuracy <- group_by(s.accuracy,participant) %>%
  do(ACC = p.accuracy %+% ., 
     ACC_F = p.accuracy + facet_wrap(~participant))
#f.accuracy$ACC_F[[1]]

### RT - MEAN ##################################################################
s.RT.M<-
  my.data %>% 
  filter(conf>0.1) %>%
  group_by(participant2,c_choice)%>%
  summarise(meanCorr=mean(key_resp_direction.rt,na.rm=T),
            sd_cor=sd(key_resp_direction.rt),
            N=n(),
            se_cor= sd(key_resp_direction.rt)/ sqrt(n()),
            ymin=mean(key_resp_direction.rt)-sd(key_resp_direction.rt)/ sqrt(n()),
            ymax=mean(key_resp_direction.rt)+sd(key_resp_direction.rt)/ sqrt(n()))

p.RT.M <- 
  ggplot(aes(y=meanCorr,x=c_choice,fill=c_choice),data=s.RT.M) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("RT") + xlab("")+theme_classic() +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.3)


f.RT.M <- group_by(s.RT.M,participant2) %>%
  do(RT.M = p.RT.M %+% .,
     RT.M_F = p.RT.M + facet_wrap(~participant2,scales="free_y"))
f.RT.M$RT.M_F[[1]]

### RT - DISTRIBUTION ###########################################################
p.RT.D <- 
  ggplot(aes(x=zRT,fill=c_choice),data=my.data) + 
  geom_density(alpha=0.2)

f.RT.D <- group_by(my.data,participant)%>%
  do(RT.D = p.RT.D %+% .,
     RT.D_F = p.RT.D + facet_wrap(~participant))
#f.RT.D$RT.D_F[[1]]

### CONFIDENCE - MEAN ###########################################################
s.conf.M<-
  my.data %>% 
  filter(conf>0.1)%>%
  filter(participant2!=115)%>%
  group_by(c_choice)%>%
  summarise(meanCorr=mean(zConf,na.rm=T),
            sd_cor=sd(zConf),
            N=n(),
            se_cor= sd(zConf)/ sqrt(n()),
            ymin=mean(zConf)-sd(zConf)/ sqrt(n()),
            ymax=mean(zConf)+sd(zConf)/ sqrt(n()))


p.conf.M <- 
  ggplot(aes(y=meanCorr,x=c_choice,fill=c_choice),data=s.conf.M) + 
  geom_bar(stat='identity',position="dodge")+
  ylab("Confidence") + xlab("")+theme_classic() +
  geom_hline(yintercept=0) + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.3)


f.conf.M <- group_by(s.conf.M,participant2) %>%
  do(conf.M = p.conf.M %+% .,
     conf.M_F = p.conf.M + facet_wrap(~participant2,scales="free_y"))
f.conf.M$conf.M_F[1]

### CONFIDENCE - DISTRIBUTION ###################################################
s.conf.D<-
  my.data %>% 
  filter(conf>0.1)

p.conf.D <- 
  ggplot(aes(x=zConf,fill=c_choice),data=s.conf.D) + 
  geom_density(alpha=0.2) 

f.conf.D <- group_by(s.conf.D,participant2)%>%
  do(conf.D = p.conf.D %+% .,
     conf.D_F = p.conf.D + facet_wrap(~participant2))
f.conf.D$conf.D_F[1]


### PUT ALL PLOTS INTO A SINGLE TABLE #################################################

# create a list with all data.frames to be merged
f.all <- list(f.accuracy,f.conf.D,f.conf.M,f,f.RT.D,f.RT.M)

# use reduce to call function merge (merges two data.frames)
basic_plots <- Reduce(function(...) merge(...,by = "participant", all = TRUE), f.all)

# save it
save(basic_plots,file='!Data/analysis_behavioural_basics.RData')




