#####################################
# READ IN EYE TRACKING FILE
####################################

# prepare main file (my.data) to hold variables associated with eye gaze
# the following variables will be written into my.data for each trial
# 1. duration left, right, middle non fixation. ratio of left and right
# 2. first fixation and last fixation changes between left and right

### CLEAR WORKSPACE ###########################################################

#rm(list=ls())

### PRE-REQS ##################################################################

# get the functions and libraries
source("functions_essentials.R")
source("functions_eyetrack.R")

# participants to exclude (if present use those generated in the read_in_data)
if (!exists("part.excl")){
  part.excl<-c(99999)
}

# If my.data is not present,source them from read_in_data
if (!exists("my.data")){
  source("read_in_behavioural.R")
}

### READ IN EYE TRACKING FILES ######################################################

# get dropbox folder
db.folder<-get.dropbox.folder()

# current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_no_social\\data",sep=""))
data.folder<-"C:\\Users\\Heekeren\\Downloads\\arrow_no_social\\arrow_no_social\\Data"


# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='gaze.txt',full.names=T)

# exclude participants
for(i in 1: length(part.excl)){
  helper<-str_detect(files.eye,pattern=paste(part.excl[i],"*",sep=""))
  files.eye<-files.eye[!helper]
}
rm(helper)

### READ IN EYE TRACK DATA #####################################################

# create containers for data frames
eye.data.trials<-list()
eye.data.fixations<-list()
eye.data.analysis<-list()
eye.participant<-NULL

eye.participant<-as.numeric(str_extract(files.eye,"[[:digit:]]{4,5}"))

for(i in 1:length(files.eye)){

  print(eye.participant[i])
  
  #Create a file.txt that is then read by the next function
  clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  eye.data.trials[[i]]<-trial_eye_track_data()
  
  eye.data.fixations[[i]]<-fixation_eye_track_data(eye.data.trials[[i]])
  
  #get finished analysis file
  eye.data.analysis[[i]]<-calc_eye_tracker_values(eye.data.fixations[[i]])
}

### ADD ANALYSED EYE TRACK DATA TO MY.DATA ######################################

namevector<-names(eye.data.analysis[[i]])
nv<-which(names(my.data)%in%namevector)
my.data[,(namevector):=NULL]
my.data[,(namevector):=list(0,'','',0,0,0,0,0,0)]

for (i in 1:length(files.eye)){
  sel<-which(my.data$participant==eye.participant[i])
  my.data[sel,(namevector):=eye.data.analysis[[i]]]
}

### ADD NEW COLUMNS ############################################################

# difference between correct and incorrect dwell times
my.data$dt_diff<-ifelse(my.data$correct=="left",my.data$duration_left-my.data$duration_right,my.data$duration_right-my.data$duration_left)

# dwell times for correct/incorrect grates
my.data$dwell_correct<-ifelse(my.data$correct=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_incorrect<-ifelse(my.data$correct=="left",my.data$duration_right,my.data$duration_left)

# dwell times for chosen/unchosen grates
my.data$dwell_chosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_left,my.data$duration_right)
my.data$dwell_unchosen<-ifelse(my.data$key_resp_direction.keys=="left",my.data$duration_right,my.data$duration_left)

# Was the first fixation correct? - what does it do with NA responses?
my.data$first_fix_correct <- ifelse(my.data$first_fix==my.data$correct,1,0)

### SAVE FILE ##################################################################
save(my.data,file='ce_eyetracking.RData') 

