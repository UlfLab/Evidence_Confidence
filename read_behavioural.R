############################
# READ IN BEHAVIOURAL DATA
############################

# Reads in behavioural data from a relevant .csv file, 
# adds a few additional columns useful for further analysis
# Saves the file

### CLEAR WORKSPACE ###################################################################

#rm(list=ls())

### PRE-REQs ##########################################################################

# get the functions and libraries
source("functions_essentials.R")

#get dropbox folder
db.folder<-get.dropbox.folder()

#current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_no_social\\Data",sep=""))

# participants to exclude
part.excl<-c(1221,7671,2292,4331,4371)
  # 1221 - too high accuracy
  # 7671 - task crashed
  # 2192,4371 - lots of trials lost during ET analysis
  # 4331 - no ET

### READ IN BEHAVIOURAL DATA ##########################################################



# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase2.csv',full.names=T)
my.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

# exclude participants
my.data <-subset(my.data,!participant %in% part.excl)

### ADD NEW COLUMNS #############################################################################

# CONF - confidence corrected  for left and right choice
my.data$conf<-my.data$PDW.response
my.data$conf<-(ifelse(my.data$conf<=0.5,0.5-my.data$conf,my.data$conf-0.5))

# ZCONF - Normalized confidence ratings and put them into a new column
my.data[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# C_CHOICE - CORRECT or INCORRECT response
my.data$c_choice<-factor(my.data$key_resp_direction.corr,labels=c("incorrect","correct"))

# zRT - normalized RT (????)
my.data[,zRT:=scale(as.numeric(key_resp_direction.rt,na.rm=T)),by=participant]

### SAVE IT ###############################################################################


save(my.data,file='!Data/ce_behavioural.RData') 
