### Phase 1 #######

### Read in data ###############
# Reads in behavioural and demographic data from relevant .csv files, 
# adds few additional columns useful for further analysis, and saves the files

#############################################################################

### PRE-REQs 

# get the functions and libraries
source("f_essentials.R")
source("f_eyetrack.R")

#current data directory (on dropbox)
data.folder<-"C:\\Users\\Majda\\ownCloud\\Confidence_Task_Magda\\Confidence_grates\\Versions of the Task\\arrow_no_social\\Data_cleanedUp"

# participants to exclude
part.excl<-c(1761,3313,4733,4734,5223,7451,7452,9931)
# lost files- need to find them

################################################################################

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase1.csv',full.names=T)
my.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

my.data <-subset(my.data,!participant %in% part.excl)

### ADD NEW COLUMNS

#participant ID across sessions
my.data$participant2<-substring(my.data$participant,1,3)

### SAVE IT
save(my.data,file='!Data\\phase1_behaviour.RData') 

### Put it all 3 staircases into a single column ############################################

my.data.s<-
  my.data %>% 
  mutate(name=ifelse(intro.name!="","drop",ifelse(trials.name!="","staircase","baseline")),
         contrast=ifelse(!is.na(intro.intensity),intro.intensity,ifelse(!is.na(trials.intensity),trials.intensity,contrast_baseline)),
         resp.key=ifelse(response_drop.keys!="",response_drop.keys,ifelse(key_resp_dir.keys!="",key_resp_dir.keys,key_resp_5.keys)),
         resp.corr=ifelse(!is.na(intro.response),intro.response,ifelse(!is.na(trials.response),trials.response,key_resp_5.corr))) %>% 
  select(name,contrast,resp.key,resp.corr,participant,participant2,correct)

# SAVE IT
save(my.data.s,file='!Data\\phase1_concat.RData') 

### Add eyetracking ########################################################################

# participants to exclude (if present use those generated in the read_in_data)
if (!exists("part.excl")){part.excl<-c(9999)}

# If my.data is not present,source them from read_in_data
# if (!exists("my.data.s")){source("read_in_behavioural.R")}

### READ IN EYE TRACKING FILES ######################################################

# current data directory (on dropbox)
data.folder<-"C:\\Users\\Majda\\ownCloud\\Confidence_Task_Magda\\Confidence_grates\\arrow_no_social\\Data_cleanedUp\\p1_et"

# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='p1_gaze.txt',full.names=T)

# exclude participants
for(i in 1: length(part.excl)){
  helper<-str_detect(files.eye,pattern=paste(part.excl[i],".",sep=""))
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
nv<-which(names(my.data.s)%in%namevector)
my.data.s[,(namevector):=NULL]
my.data.s[,(namevector):=list(0,'','',0,0,0,0,0,0)]

for (i in 1:length(files.eye)){
  sel<-which(my.data.s$participant==eye.participant[i])
  my.data.s[sel,(namevector):=eye.data.analysis[[i]]]
}

### ADD NEW COLUMNS ############################################################

# difference between correct and incorrect dwell times
my.data.s$dt_diff<-ifelse(my.data.s$first_fix=="left",my.data.s$duration_left-my.data.s$duration_right,my.data.s$duration_right-my.data.s$duration_left)

# Was the first fixation correct? - what does it do with NA responses?
my.data.se$first_correct <- ifelse(my.data.se$first_fix==my.data.se$correct,1,0)


### SAVE FILE ##################################################################
save(my.data.s,file='!Data\\phase1_eyetracking.RData') 

