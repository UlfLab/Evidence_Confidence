### Read in Phase 1 - staircase procedure #######

# Reads in behavioural data from relevant phase 1 (staircase procedure) .csv files, 
# adds few additional columns useful for further analysis, and saves the files

#############################################################################

# LOST FILES
# c(1761,3313,4733,4734,5223,7451,7452)


################################################################################

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase1.csv',full.names=T)
my.data.p1f<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 


### SAVE IT
save(my.data.p1f,file='_Data\\d_staircase_bf.RData') 

### Put it all 3 staircases into a single column ############################################

my.data.p1<-
  my.data.p1f %>% 
  mutate(name=ifelse(intro.name!="","drop",ifelse(trials.name!="","staircase","baseline")),
         contrast=ifelse(!is.na(intro.intensity),intro.intensity,ifelse(!is.na(trials.intensity),trials.intensity,contrast_baseline)),
         response.keys=ifelse(response_drop.keys!="",response_drop.keys,ifelse(key_resp_dir.keys!="",key_resp_dir.keys,key_resp_5.keys)),
         response.corr=ifelse(!is.na(intro.response),intro.response,ifelse(!is.na(trials.response),trials.response,key_resp_5.corr)),
         partSes_ID = participant,
         part_ID = substring(participant,1,3)) %>% 
  select(name,contrast,response.keys,response.corr,partSes_ID,part_ID)

# SAVE IT
save(my.data.p1,file='_Data\\d_staircase_b.RData') 


### READ IN EYE TRACKING FILES ######################################################

# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='p1_gaze.txt',full.names=T)

### READ IN EYE TRACK DATA #####################################################

# create containers for data frames
eye.data.trials.p1<-list()
eye.data.fixations.p1<-list()
eye.data.analysis.p1<-list()
eye.participant.p1<-NULL

eye.participant<-as.numeric(str_extract(files.eye,"[[:digit:]]{4,5}"))

for(i in 1:length(files.eye)){
  
  print(eye.participant[i])
  
  #Create a file.txt that is then read by the next function
  clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  eye.data.trials.p1[[i]]<-trial_eye_track_data()
  
  eye.data.fixations.p1[[i]]<-fixation_eye_track_data(eye.data.trials.p1[[i]])
  
  #get finished analysis file
  eye.data.analysis.p1[[i]]<-calc_eye_tracker_values(eye.data.fixations.p1[[i]])
}

### ADD ANALYSED EYE TRACK DATA TO MY.DATA ######################################
my.data.p1t <- my.data.p1

namevector<-names(eye.data.analysis.p1[[i]])
nv<-which(names(my.data.p1t)%in%namevector)
my.data.p1t[,(namevector):=NULL]
my.data.p1t[,(namevector):=list(0,'','',0,0L,0L,0L,0L)]

for (i in 1:length(files.eye)){
  sel<-which(my.data.p1t$partSes_ID==eye.participant[i])
  my.data.p1t[sel,(namevector):=eye.data.analysis.p1[[i]]]
}

### SAVE FILE ##################################################################
save(my.data.p1t,file='_Data\\d_staircase_bt.RData') 
save(eye.data.analysis.p1,file='_Data\\d_staircase_tf_analysis.RData') 
save(eye.data.fixations.p1,file='_Data\\d_staircase_tf_fixations.RData') 
save(eye.data.trials.p1,file='_Data\\d_staircase_tf_trials.RData') 
