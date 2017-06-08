# staircase-
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_staircase\\data",sep=""))

files<-list.files(path=data.folder,pattern='*staircase_phase1.csv',full.names=T)
s.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

a<-
  s.data %>% 
 # filter(participant=="m4",!contrast_bas%in%c(0.015,0.13)) %>% 
  group_by(contrast_bas) %>% 
  summarise(corr= sum(key_resp_5.corr),prob=mean(key_resp_5.corr),n=n())%>% 
  arrange(contrast_bas)

c<-quickpsy(d=as.data.frame(a),x=contrast_bas,k=corr,guess = 0.5, n=n,lapses=TRUE,prob=0.7)

ggplot(aes(x=contrast_bas,y=prob),data=a)+geom_point()+
  geom_line(data= c$curves, aes(x = x, y = y))

a$e <- pweibull(a$contrast_bas, shape=1.2, scale = 0.0469)
a$e <- pweibull(a$contrast_bas, shape=3.5, scale = 0.0469)
ggplot(aes(x=contrast_bas,y=prob),data=a)+geom_point()+
  geom_point(data= a, aes(x = contrast_bas, y = e,col="blue"))

a<-
  s.data %>% 
  filter(participant=="m4",!contrast_bas%in%c(0.015,0.13)) %>% 
  group_by(contrast_bas) %>% 
  summarise(n=n(),nYes= sum(key_resp_5.corr),nNo=n()-sum(key_resp_5.corr) ,prob=mean(key_resp_5.corr))%>% 
  arrange(contrast_bas)

model <- glm(cbind(nYes, nNo) ~ contrast_bas, data= a, family = binomial(logit))

xseq <- seq(0, 0.15, .01)

yseq <- predict(model, data.frame(contrast_bas = xseq), type = 'response')

curve <- data.frame(xseq, yseq)

ggplot(aes(x=contrast_bas,y=prob),data=a)+geom_point()+
  geom_line(data= curve, aes(x = xseq, y = yseq,col="blue"))


test<-
  s.data %>% 
  filter(participant %in% c("p1","p2","m5")) %>%  
  select(contrast_bas,key_resp_5.corr,participant) 

test2<-
  test %>% 
  group_by(participant,contrast_bas) %>% 
  summarise(corr= sum(key_resp_5.corr),prob=mean(key_resp_5.corr),n=n())%>% 
  arrange(contrast_bas)


b<-quickpsy(d=as.data.frame(test),x=contrast_bas,k=key_resp_5.corr,guess = 0.5,lapses=0.01,prob=0.7, fun=weibull_fun)
c<-quickpsy(d=as.data.frame(s.data),x=contrast_bas,k=key_resp_5.corr,guess = 0.5,lapses=TRUE, fun=logistic_fun)
plotcurves(b)

ggplot(aes(x=contrast_bas,y=prob,col=participant,shape=participant),data=test2)+geom_point()
