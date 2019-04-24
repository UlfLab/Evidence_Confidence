######################
# FIT META D' TO DATA - BASED ON SAMPLING BIAS
#######################


### PRE-ALLOCATION ################################################
pID <-unique(my.data.a$partSes_ID)

m.meta.b.md <- matrix(NA, ncol = 5, nrow = length(pID))
m.meta.b.d <- matrix(NA, ncol = 5, nrow = length(pID))
                   
exclude.part <- c(1174,1762,4712,4721,4723,5221,5223,7455,7511,7462,8851,9932)

### CALCULALTE META-D' and D' #####################################

for (ipart in 1:length(pID)){
  
  print(pID[ipart])
  
 # if(pID[[ipart]]%in%exclude.part){next}
  
  for (iBias in 1:5){
    
    f.data <- 
      my.data.a %>% 
      filter(partSes_ID==unique(my.data.a$partSes_ID)[ipart]) %>% 
  #    mutate(bin_bias_meta = ntile(bias,3)) %>% 
      filter(my.data.a$bin_bias_ch==iBias)
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta.b.md[ipart,iBias] <- mean(as.numeric(model$meta_d))
    m.meta.b.d[ipart,iBias] <- mean(as.numeric(model$d1))
  }
}

### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
meta.b.md <- data.frame(m.meta.b.md[,1:5])
meta.b.d <- data.frame(m.meta.b.d[,1:5])

colnames(meta.b.md)<-c("bias1","bias2","bias3","bias4","bias5")
colnames(meta.b.d)<-c("bias1","bias2","bias3","bias4","bias5")

meta.b.md$partSes_ID<-pID
meta.b.d$partSes_ID<-pID

# put all biases into one colums
d.bias<-
  meta.b.d%>% 
  gather(bias, d, bias1, bias2, bias3, bias4, bias5)

meta.bias<-
  meta.b.md %>% 
  gather(bias, meta_D, bias1, bias2, bias3, bias4, bias5) %>% 
  mutate(d = d.bias$d) %>% 
  mutate(mD_d =  meta_D-d,
         mD_ratio = meta_D/d,
         bias_n = as.double(substring(bias,5,5)))


save(meta.bias,file='_Data\\d_mainTask_mf_bias.RData')

### PLOT DATA #####################################################

# META-D'
ggplot(aes(x=as.factor(bias), y=mD_ratio),data=meta.bias) + 
  geom_boxplot()+   xlab("") + ylab("meta-d - d") 

summary(lm(mD_d ~ as.factor(bias_n), data=meta.bias))

summary(lmer(zConf ~ zBias_ch*choice+(1|part_ID:session), data=my.data.a))
plot_model(lmer(zConf ~ zBias_ch*choice+(1|part_ID:session), data=my.data.a))


# BIAS
bias.m<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_bias_ch,choice) %>% 
  summarise_at(vars(zConf,bias_ch),list(mean=mean))

#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_bias_ch),fill=choice),data=bias.m)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("sampling time") + theme_bw()

### Time ################################################
pID <-unique(my.data.a$partSes_ID)

m.meta.t.md <- matrix(NA, ncol = 5, nrow = length(pID))
m.meta.t.d <- matrix(NA, ncol = 5, nrow = length(pID))

exclude.part <- c(1174,1762,4712,4714,4721,4723,5221,5223,7455,7511,7462,8851,9932)

### CALCULALTE META-D' and D' #####################################

for (ipart in 1:length(pID)){
try({
  print(pID[ipart])
  
 # if(pID[[ipart]]%in%exclude.part){next}
  
  for (iTime in 1:5){
    f.data <- 
      my.data.a %>% 
      filter(partSes_ID==unique(my.data.a$partSes_ID)[ipart]) %>% 
      filter(my.data.a$bin_time==iTime)
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta.t.md[ipart,iTime] <- mean(as.numeric(model$meta_d))
    m.meta.t.d[ipart,iTime] <- mean(as.numeric(model$d1))
  }})
}
### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
meta.t.md <- data.frame(m.meta.t.md[,1:5])
meta.t.d <- data.frame(m.meta.t.d[,1:5])

colnames(meta.t.md)<-c("time1","time2","time3","time4","time5")
colnames(meta.t.d)<-c("time1","time2","time3","time4","time5")


meta.time<-
  meta.t.d %>% 
  gather(time, meta_D,-c()) %>% 
  mutate(d = d.bias$d) %>% 
  mutate(mD_d =  meta_D- d)


meta.t.md$partSes_ID<-pID
meta.t.d$partSes_ID<-pID

# put all biases into one colums
d.time<-
  meta.t.d%>% 
  gather(time, d, time1, time2, time3, time4, time5)

meta.time<-
  meta.t.md %>% 
  gather(time, meta_D, time1, time2, time3, time4, time5) %>% 
  mutate(d = d.bias$d) %>% 
  mutate(mD_d =  meta_D- d,
         time_n = as.double(substring(time,5,5)))

# GRAPH - META
ggplot(aes(x=as.factor(time), y=mD_d),data=meta.time) + 
  geom_boxplot()+   xlab("") + ylab("meta-d - d")

plot.time<-lm(mD_d~as.factor(time_n),data=meta.time)
plot_model(plot.time,show.values = TRUE,show.p=TRUE)

summary(lm(mD_d ~ as.factor(time_n), data=meta.time))

# GRAPH - BINS

time.m<-
  as.data.frame(my.data.a) %>%
  group_by(partSes_ID,bin_time,choice) %>% 
  summarise_at(vars(zConf,time,response.corr),list(mean=mean))

#graph - boxplot
ggplot(aes(y=zConf_mean,x=as.factor(bin_time),fill=choice),data=time.m)+
  geom_boxplot() +
  ylab("zConfidence") + xlab("sampling time") + theme_bw() 


save(meta.time,file='_Data\\d_mainTask_mf_time.RData')




