#####################################
# READ IN EYE TRACKING FILE
####################################

# prepare main file (my.data) to hold variables associated with eye gaze
# the following variables will be written into my.data for each trial
# 1. duration: left, right, centre, non fixation. ratio of left and right
# 2. first fixation and last fixation changes between left and right

### CLEAR WORKSPACE ###########################################################

#rm(list=ls())

### PRE-REQS ##################################################################

# needs my.data file (from r_behaviour)

### READ IN EYE TRACKING FILES ######################################################

# get names of the eye tracking files
files.eye<-list.files(path=data.folder,pattern='[[:digit:]]{4}_gaze.txt',full.names=T)

### READ IN EYE TRACK DATA #####################################################

# create containers for data frames
eye.data.trials<-list()
eye.data.fixations<-list()
eye.data.analysis<-list()
eye.participant<-NULL

eye.participant<-as.numeric(str_extract(files.eye,"[[:digit:]]{4}"))

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
my.data[,(namevector):=NULL]
my.data[,(namevector):=list(0,'','',0,0L,0L,0L,0L)]

for (i in 1:length(files.eye)){
  sel<-which(my.data$partSes_ID==eye.participant[i])
  my.data[sel,(namevector):=eye.data.analysis[[i]]]
}

### ADD NEW COLUMNS ############################################################

# mark trials where subject did not look to the left or right grating
my.data$dt_diff<-my.data$dwell_left-my.data$dwell_right

### SAVE FILE ##################################################################
save(my.data,file='_Data\\d_mainTask_bt.RData') 
save(eye.data.analysis,file='_Data\\d_mainTask_tf_analysis.RData') 
save(eye.data.fixations,file='_Data\\d_mainTask_tf_fixations.RData') 
save(eye.data.trials,file='_Data\\d_mainTask_tf_trials.RData') 
