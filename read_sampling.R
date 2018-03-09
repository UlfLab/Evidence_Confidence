###############################################
# READ IN EYE TRACKING FILE - SAMPLING PATTERNS
###############################################
# Saves information about gaze position during trials into a R friendly format
# includes information about - gaze position & pupil diameter per 4ms 
# also contains the trial number/participant ID

### CLEAR WORKSPACE ###########################################################

#rm(list=ls())

### PRE-REQS ##################################################################

# get the functions and libraries
source("functions_essentials.R")
source("functions_eyetrack.R")

# If eye.data.fixations is not present,source it from read_in_eyetrack
if (!exists("eye.data.fixations")){
  source("read_eyetrack.R")
}

# participants to exclude (if present use those generated in the read_in_data)
if (!exists("part.excl")){
  part.excl<-c(99999)
}



### READ IN EYE TRACKING FILES ###############################################
# get dropbox folder
db.folder<-get.dropbox.folder()

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

### READ IN EYE DATA - SAMPLING PATTERNS ###################################

# create containers for data
eye.participant<-NULL
eye.trial.data<-list()

for(i in 1:length(files.eye)){
  print(eye.participant[i])

  eye.trial.data[[i]]<-eye_tracker_trialFixation(eye.data.fixations[[i]])
  eye.trial.data[[i]]$participant<-eye.participant[i]
  
}

my.eye<-rbindlist(eye.trial.data,use.names=TRUE,fill=TRUE)

### SAVE IT ################################################################
save(my.eye,file="!Data/ce_sampling.RData")
