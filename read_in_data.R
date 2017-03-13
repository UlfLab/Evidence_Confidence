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
db.folder<-get.dropbox.folder()

#current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_CURRENT\\Data",sep=""))

# participants to exclude
part.excl<-c(99999)

################################################################################

### READ IN PHASE 2

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase2.csv',full.names=T)
my.data.P2<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

# exclude participants
my.data.P2<-subset(my.data.P2,!participant %in% part.excl)

### ADD NEW COLUMNS

# CONF - confidence corrected  for left and right choice
my.data.P2$conf<-my.data.P2$PDW.response
my.data.P2$conf<-(ifelse(my.data.P2$conf<=0.5,0.5-my.data.P2$conf,my.data.P2$conf-0.5))

# ZCONF - Normalized confidence ratings and put them into a new column
my.data.P2[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# C_CHOICE - CORREcT or INCORRECT response
my.data.P2$c_choice<-factor(my.data.P2$key_resp_direction.corr,labels=c("incorrect","correct"))

# zRT - normalized RT (????)
my.data.P2[,zRT:=scale(as.numeric(key_resp_direction.rt,na.rm=T)),by=participant]

### BASIC STATS

# how many trials without answer 
# do not these delete trials as trials will be matched to eye tracking data
no.ans <- sum(is.na(my.data.P2$key_resp_direction.rt))

# Calculate the number or participants
no.part<-length(unique(my.data.P2$participant))

### SAVE IT
save(my.data.P2,file='ce_data_P2.RData') 

################################################################################
### READ IN PHASE 3

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase3_lr.csv',full.names=T)
my.data.P3<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

# exclude participants
my.data.P3<-subset(my.data.P3,!participant %in% part.excl)

### ADD NEW COLUMNS

# CONF - confidence corrected  for left and right choice
my.data.P3$conf<-my.data.P3$PDW.response
my.data.P3$conf<-(ifelse(my.data.P3$conf<=0.5,0.5-my.data.P3$conf,my.data.P3$conf-0.5))

# ZCONF - Normalized confidence ratings and put them into a new column
my.data.P3[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# C_CHOICE - CORREcT or INCORRECT response
my.data.P3$c_choice<-factor(my.data.P3$key_resp_direction.corr,labels=c("incorrect","correct"))

# zRT - normalized RT (????)
my.data.P3[,zRT:=scale(as.numeric(key_resp_direction.rt,na.rm=T)),by=participant]

### BASIC STATS

# how many trials without answer 
# do not these delete trials as trials will be matched to eye tracking data
no.ans <- sum(is.na(my.data.P3$key_resp_direction.rt))

# Calculate the number or participants
no.part<-length(unique(my.data.P3$participant))

### SAVE IT
save(my.data.P3,file='ce_data_P3.RData') 


###############################################################################

### READ IN DEMOGRAPHIC DATA

# Extract data from Participants_Fragebögen and save as my.demographics
files<-list.files(path=data.folder,pattern='*gen.csv',full.names=T)
my.demo<-data.frame(lapply(files, fread))

# Delete unwanted columns
keep <- c("ID.used","Alter","Geschlecht")
my.demo <- my.demo[keep]

# SAVE IT
save(my.demo,file='ce_demographics.RData')
