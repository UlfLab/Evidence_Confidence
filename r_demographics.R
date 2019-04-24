############################
# READ IN DEMOGRAPHIC DATA
############################

# Reads in participants' demographic data collected at the beginning of the study
# Adds the demographic data to my.data.a (contains behavioural and eyetracking data that survived exclusion)


# Extract data from Participants_Fragebögen and save as my.demographics
files<-list.files(path=data.folder,pattern='long.csv',full.names=T)
my.demo.f<-data.frame(lapply(files, fread))

# Delete unwanted columns and and translate column names into English

my.demo <-
  subset(my.demo.f, select = c("ID","Alter","Geschlecht")) %>% 
  mutate(part_ID=substring(ID,1,3),
         ID = NULL)

colnames(my.demo)<-c("age","sex","part_ID")

# add to my.data.a
my.data.d<-merge(my.data.a,my.demo,by = "part_ID", all = TRUE)

# SAVE
save(my.demo.f,file='_Data/d_mainTask_df.RData')
save(my.demo,file='_Data/d_mainTask_d.RData')
save(my.data.d,file='_Data/d_mainTask_btead.RData')


