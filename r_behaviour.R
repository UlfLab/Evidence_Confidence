############################
#READ IN BEHAVIOURAL DATA
############################

# Reads in behavioural data from relevant .csv files, 
# adds few additional columns useful for further analysis, and saves the file


### READ IN PHASE 2 ###############################

files<-list.files(path=data.folder,pattern='*phase2.csv',full.names=T)
my.data.f<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

### EXCLUDE COLUMNS THAT ARE NOT NEEDED FOR THE ANALYSIS ###############################
my.data <- subset(my.data.f, select = c(Ltilt,Rtilt,op_high,op_low,contrast,correct,trials.thisN,key_resp_direction.corr,key_resp_direction.rt,key_resp_direction.keys,participant,PDW.response))

### RENAME COLUMNS ###############################
setnames(my.data, 
         old=c("key_resp_direction.corr","key_resp_direction.rt","key_resp_direction.keys","participant","trials.thisN"), 
         new=c("response.corr", "response.rt","response.keys","partSes_ID","trial"))

### ADD NEW COLUMNS ###############################
my.data<-
  my.data %>% 
  mutate(conf = ifelse(PDW.response<=0.5,0.5-PDW.response,PDW.response-0.5),
         choice = factor(response.corr,labels=c("incorrect","correct")),
         session=ifelse(substring(partSes_ID,4,4)>2,2,1),
         session = ifelse(partSes_ID==7473,1,session), # phase 1 was run 3x in the first session for 747
         part_ID=substring(partSes_ID,1,3))

### SAVE ###############################
save(my.data.f,file='_Data\\d_mainTask_bf.RData') 
save(my.data,file='_Data\\d_mainTask_b.RData') 

rm(my.data.f)
