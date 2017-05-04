#####################################
# READ IN EYE TRACKING FILE
####################################

# prepare main file (my.data) to hold variables associated with eye gaze
# the following variables will be written into my.data for each trial
# 1. duration left, right, middle non fixation. ratio of left and right
# 2. first fixation and last fixation changes between left and right

#####################################################################

# clear the workspace
rm(list=ls())

#####################################################################

### PRE-REQs 

# get the functions and libraries
source("read_in_data.R")
source("essential_functions_libraries.R")
source("eye_tracker_functions.R")


# get dropbox folder
db.folder<-get.dropbox.folder()

# participants to exclude (if present use those generated in the read_in_data)
if (!exists("part.excl")){
  part.excl<-c(99999)
}

#####################################################################

### READ IN EYE TRACK DATA

# current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_no_social\\data",sep=""))

# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='gaze.txt',full.names=T)

# exclude participants
for(i in 1: length(part.excl)){
  helper<-str_detect(files.eye,pattern=paste(part.excl[i],"*",sep=""))
  files.eye<-files.eye[!helper]
}
rm(helper)

# create containers for data frames
eye.data.list<-list()
eye.data.list2<-list()
eye.analysis.list<-list()

# get the participant ID (which is 4-5 digits long)
eye.participant<-NULL
for(i in 1:length(files.eye)){
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  print(eye.participant[i])
  
  #Create a file.txt that is then read by the next function
  clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  eye.data.list[[i]]<-trial_eye_track_data()
  
  eye.data.list2[[i]]<-fixation_eye_track_data(eye.data.list[[i]])
  
  #get finished analysis file
  eye.analysis.list[[i]]<-calc_eye_tracker_values(eye.data.list2[[i]])
}

# Add these to my.data
namevector<-names(eye.analysis.list[[1]])
nv<-which(names(my.data)%in%namevector)
my.data[,(namevector):=NULL]
my.data[,(namevector):=list(0,'','',0,0,0,0,0,0,0)]


# SAVE
save(my.data,file='ce_etData.RData') 

# MAKE THE EYE.DATA INTO A DATA.FRAME AS WELL and add to it the timing of fixations
for (i in 1:length(files.eye)){
  sel<-which(my.data$participant==eye.participant[i])
  my.data[sel,(namevector):=eye.analysis.list[[i]]]
}

eye.trial.data<-list()

for(i in 1:length(files.eye)){
  print(eye.participant[i])
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  
  eye.trial.data[[i]]<-eye_tracker_trialFixation(eye.data.list2[[i]])
  eye.trial.data[[i]]$participant<-eye.participant[i]

}

my.eye<-rbindlist(eye.trial.data,use.names=TRUE,fill=TRUE)

save(my.eye,file="ca_ET_trial.RData")
