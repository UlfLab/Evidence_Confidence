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
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_no_social\\Data",sep=""))

# participants to exclude
part.excl<-c(7671,2292)
  # 7671 - task crashed
  # 2192 - lots of trials lost during ET analysis
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
my.data[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# C_CHOICE - CORREcT or INCORRECT response
my.data$c_choice<-factor(my.data$key_resp_direction.corr,labels=c("incorrect","correct"))

# zRT - normalized RT (????)
my.data[,zRT:=scale(as.numeric(key_resp_direction.rt,na.rm=T)),by=participant]

### BASIC STATS

# how many trials without answer 
#no.ans <- sum(is.na(my.data$key_resp_direction.rt))

# Calculate the number or participants
#no.part<-length(unique(my.data$participant))

### SAVE IT
save(my.data,file='ce_data.RData') 

################################################################################
