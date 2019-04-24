###############################################
# READ IN EYE TRACKING FILE - SAMPLING PATTERNS
###############################################
# Saves information about length of each fixation in the trial  

### PRE-REQS ##################################################################

# requires eye.data.fixations (from r_eyetrack.R) and my.data (behaviour + eyetrack)

### READ IN EYE TRACKING FILES ###############################################

# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='[[:digit:]]{4}_gaze.txt',full.names=T)

### READ IN EYE DATA - SAMPLING PATTERNS ###################################

# create containers for data
eye.participant<-NULL
eye.participant<-as.numeric(str_extract(files.eye,"[[:digit:]]{4}"))
eye.fix.data<-list()

for(i in 1:length(files.eye)){
  print(eye.participant[i])
  
  eye.fix.data[[i]]<-calc_length_IndFix(eye.data.fixations[[i]])
  eye.fix.data[[i]]$partSes_ID<-eye.participant[i]
  
}

my.eye.fix<-rbindlist(eye.fix.data,use.names=TRUE,fill=TRUE)

### SAVE IT ################################################################
save(my.eye.fix,file="_Data/d_mainTask_sf.RData")
