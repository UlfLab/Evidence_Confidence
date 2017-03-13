# MAKE THE EYE.DATA INTO A DATA.FRAME AS WELL and add to it the timing of fixations
eye.trial.data<-list()

for(i in 1:length(files.eye)){
  print(eye.participant[i])
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  
  eye.trial.data[[i]]<-eye_tracker_fixation(eye.data.list[[i]])
  eye.trial.data[[i]]$participant<-eye.participant[i]
  print(eye.participant[i])
}

my.eye<-rbindlist(eye.trial.data,use.names=TRUE,fill=TRUE)

save(my.eye,file="confidence_attention_ET_trial.RData")

# save only the important bits so it is easier to deal with
my.eye.lt<-select(my.eye,V4,trial,fix,change,fixPos,fixNum,time,timeTrial,participant)

# ADD SOCIAL INFO AND CORRECT/INCORRECT - WILL MAKE THIS INTO A FUNCTION EVENTUALLY

my.eye.lt$social3<-""
my.eye.lt$c_choice<-""

# FOR EACH PARTICIPANT look up in my.data which trials have no social info (social==0),
# select those trial in the my.eye and add none to their social3 column
for (iparticipant in unique(my.eye.lt$participant)){
  
  no.info.trials <- my.data$trial[my.data$social==0&my.data$participant==iparticipant]
  my.eye.lt$social3[my.eye.lt$trial%in%no.info.trials&my.eye.lt$participant==iparticipant] <- "none"
  
  c_choice.trials <- my.data$trial[my.data$c_choice=="correct"&my.data$participant==iparticipant]
  my.eye.lt$c_choice[my.eye.lt$trial%in%c_choice.trials&my.eye.lt$participant==iparticipant] <- "correct"
  
  in_choice.trials<- my.data$trial[my.data$c_choice=="incorrect"&my.data$participant==iparticipant]
  my.eye.lt$c_choice[my.eye.lt$trial%in%in_choice.trials&my.eye.lt$participant==iparticipant] <- "incorrect"
  
}

save(my.eye.lt,file="confidence_attention_ET_trial_LT.RData")
