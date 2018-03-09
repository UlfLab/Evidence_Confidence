############################
#READ IN BEHAVIOURAL DATA
############################

# Reads in behavioural and demographic data from relevant .csv files, 
# adds few additional columns useful for further analysis, and saves the files

#############################################################################

# clear the workspace
rm(list=ls())

#############################################################################

### PRE-REQs 

# get the functions and libraries
source("essential_functions_libraries.R")

#get dropbox folder
#db.folder<-get.dropbox.folder()

#current data directory (on dropbox)
#data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_no_social\\Data",sep=""))
data.folder<-"C:\\Users\\Majda\\ownCloud\\Confidence_Task_Magda\\Confidence_grates\\Versions of the Task\\arrow_no_social\\Data_cleanedUp"


# participants to exclude
part.excl<-c(8872,8862,1152,1153,1756,6632)
  # 8872,8862 - too high accuracy > 85%
  # 1152,1153 - lots of trials with low confidence (did he get the task???)
  # 6632 - eyetracker trouble
  # 1756 - too low accuracy < 55% (in at least one of the sessions)
################################################################################

### READ IN PHASE 2

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase2.csv',full.names=T)
my.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

# exclude participants
my.data <-subset(my.data,!participant %in% part.excl)

### ADD NEW COLUMNS

# CONF - confidence corrected  for left and right choice
my.data$conf<-my.data$PDW.response
my.data$conf<-(ifelse(my.data$conf<=0.5,0.5-my.data$conf,my.data$conf-0.5))

# ZCONF - Normalized confidence ratings and put them into a new column
# my.data[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# C_CHOICE - CORREcT or INCORRECT response
my.data$c_choice<-factor(my.data$key_resp_direction.corr,labels=c("incorrect","correct"))

#participant ID across sessions
my.data$participant2<-substring(my.data$participant,1,3)

### BASIC STATS

# how many trials without answer 
no.ans <- sum(is.na(my.data$key_resp_direction.rt))

# Calculate the number or participants
no.part<-length(unique(my.data$participant))

# participants ids
b.participant <- unique(my.data$participant)

### SAVE IT
save(my.data,file='!Data\\data_behaviour.RData') 

################################################################################
