### READ IN DEMOGRAPHIC DATA

data.folder<-"C:\\Users\\Majda\\ownCloud\\Confidence_Task_Magda\\Confidence_grates\\arrow_no_social\\Data_cleanedUp"

# Extract data from Participants_Fragebögen and save as my.demographics
files<-list.files(path=data.folder,pattern='long.csv',full.names=T)
my.demo<-data.frame(lapply(files, fread))

# Delete unwanted columns
keep <- c("ID","Alter","Geschlecht")
my.demo <- my.demo[keep]
colnames(my.demo)<-c("participant","age","sex")

my.demo$participant2<-substring(my.demo$participant,1,3)
my.demo<-my.demo[,!(colnames(my.demo) == "participant")]

# SAVE IT
save(my.demo,file='!Data/data_demographics.RData')

# add to my.data.a
my.data.d<-merge(my.data.a,my.demo,by = "participant2", all = TRUE)
