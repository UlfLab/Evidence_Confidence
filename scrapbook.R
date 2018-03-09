### DO THE SAME ACTION FOR ALL PARTICIPANTS ###############

p.RT.D <- 
  ggplot(aes(x=key_resp_direction.rt,fill=c_choice),data=my.data.e) + 
  geom_density(alpha=0.2)

f.RT.D <- group_by(my.data,participant)%>%
  do(RT.D = p.RT.D %+% .,
     RT.D_F = p.RT.D + facet_wrap(~participant))


### HOW TO EXTRACT DATA FROM A VARIABLE OF THE SAME NAME AS A STRING

b<-"s.conf"
c<-eval(parse(text = b))

### GET DROPBOX FILES #############################################
get.dropbox.folder <- function() {
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  return(file_content)
}

### DWELL TIME DISTRIBUTIONS ###################

my.data.d<-
  my.data %>% 
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>% 
  gather(fixation,duration,c(duration_right,duration_left))

ggplot(aes(x=duration/1000000,fill=fixation),data=my.data.d)+ geom_density(alpha=0.2)+facet_wrap(~participant)
ggsave("!Analysis/dwellTime_distribution_pilot.png", width = 13.3, height = 7.47, units ="in")

### DWELL TIME LEFT right vs FIX ###################
my.data.d<-
  my.data %>% 
  filter(conf>0.1) %>% 
  filter(!is.na(dt_diff)) %>% 
  gather(fixation,duration,c(duration_right,duration_left,duration_fix,duration_none)) %>% 
  group_by(participant,fixation) %>% 
  summarise(mean_dwell=mean(duration,na.rm=T))

ggplot(aes(y=mean_dwell/1000000,x=fixation,fill=fixation),data=my.data.d)+ geom_bar(stat="identity")+facet_wrap(~participant)
ggsave("!Analysis/dwellTimes.png", width = 13.3, height = 7.47, units ="in")


### GAM ##########################
library(mgcv)
unique(my.data.a$participant2)
sub.data<-subset(my.data.a,participant2==unique(my.data.a$participant2)[3])
sub.data<-my.data.a
m.1<-gam(zConf~s(weight,strength_ch2),data=sub.data)
summary(m.1)
fit.data<-data.frame(weight=sub.data$weight,strength=sub.data$strength_ch2,fitted=m.1$fitted.values,conf=sub.data$zConf)
vis.gam(m.1,plot.type="contour")
m.2<-gam(key_resp_direction.corr~s(weight,strength_ch2),data=sub.data,family="binomial")
summary(m.2)
fit.data<-data.frame(weight=sub.data$weight,strength=sub.data$strength_ch2,fitted=m.2$fitted.values,value=sub.data$key_resp_direction.corr)
vis.gam(m.2,plot.type="contour")

### DATA FOR HDDM ##########################
my.data.hddm <- 
  my.data.a %>% 
  select(participant,key_resp_direction.corr,key_resp_direction.rt,strength_ch)

colnames(my.data.hddm)=c("participant"="subj_idx", "key_resp_direction.corr"="response","key_resp_direction.rt"="rt","strength_ch"="strength_chosen")
colnames(my.data.hddm)=c("participant"="subj_idx", "key_resp_direction.corr"="response","key_resp_direction.rt"="rt","strength_corr"="strength_correct")

write.csv(my.data.hddm, file = "data_ddm_ch.csv")

### GLM ##########################
sub.data<-subset(my.data.a,participant2==115)
library(arm)
scale(my.data.a$weight)
m.1<-glmer(key_resp_direction.corr~scale(weight)*strength_corr2+(1|participant2),family=binomial,data=my.data.a)
m.1<-glm(sub.data$key_resp_direction.corr~sub.data$weight+I(abs(scale(sub.data$strength_ch2))),family=binomial)
summary(m.1)
plot(my.data.a$zConf~fitted(m.1))
summary(lm(my.data.a$zConf~fitted(m.1)))

###################
#separate reaction times
data.ddm<-
  my.data %>% 
  select(participant,key_resp_direction.corr,key_resp_direction.rt,strength_conf2)

colnames(data.ddm)<-c("subj_idx","response","rt","strength_chosen")
write.csv(data.ddm,file="ce_data_ddm.csv", row.names = FALSE)


### mystery graph
d<-
  my.data %>% 
  #filter(bin_strength2==1) %>% 
  group_by(participant,bin_weight,bin_strength2) %>% 
  summarise(mean_cor=mean(key_resp_direction.corr),
            mean_conf=mean(zConf))

ggplot(aes(x=bin_weight,y=mean_cor,color=as.factor(bin_strength2)),data=d) + 
  geom_point() + 
  facet_wrap(~participant) + 
  geom_line() #+ 
#  ggtitle(paste("bin_strength =",unique(d$bin_strength2)))



### DURATION FIX vs DURATION LEFT/RIGTH ##################################################################

s.durations<-
  my.data %>% 
  filter(conf>0.1) %>%
  filter(!is.na(dt_diff)) %>%
  gather(duration_type,duration_length,duration_left,duration_right,duration_fix,duration_none) 

s.durations$duration_length[is.na(s.durations$duration_length)] <- 0

s.durations<-
  s.durations %>% 
  group_by(participant,duration_type) %>%
  summarise(meanLength=mean(duration_length),
            N=n())

p.durations<-
  ggplot(aes(y=meanLength,x=as.factor(duration_type),fill=duration_type),data=s.durations) +
  geom_bar(stat='identity',position="dodge")+facet_wrap(~participant)

# 

ff.correct<-
  my.data %>% 
  group_by(participant,correct,first_fix) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),
            N=n(),
            mean_conf=mean(zConf))

ggplot(aes(x=correct,y=mean_corr),data=ff.correct)+geom_bar(stat="identity")+facet_grid(first_fix~participant)
ggplot(aes(x=correct,y=mean_conf),data=ff.correct)+geom_bar(stat="identity")+facet_grid(first_fix~participant)
ggplot(aes(x=first_fix,y=N),data=ff.correct)+geom_bar(stat="identity")+facet_wrap(~participant)

ffc.data<-
  my.data %>% 
  group_by(participant,first_fix_correct) %>%
  summarise(mean_corr=mean(key_resp_direction.corr*100),
            N=n(),
            mean_conf=mean(zConf))

ggplot(aes(x=first_fix_correct,y=mean_corr,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=first_fix_correct,y=mean_conf,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=first_fix_correct,y=N,fill=first_fix_correct),data=ffc.data)+geom_bar(stat="identity")+facet_wrap(~participant)


lr.data<-
  my.data %>% 
  group_by(participant,key_resp_direction.keys) %>% 
  summarise(N=n(),
            mean_corr=mean(key_resp_direction.corr),
            mean_conf=mean(zConf))

ggplot(aes(x=key_resp_direction.keys,y=N),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=key_resp_direction.keys,y=mean_corr),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)
ggplot(aes(x=key_resp_direction.keys,y=mean_conf),data=lr.data)+geom_bar(stat="identity")+facet_wrap(~participant)

#################x

my.data<-
  my.data %>% 
  filter(participant2!=175)


test<-
  my.data %>% 
  filter(!is.na(conf))
