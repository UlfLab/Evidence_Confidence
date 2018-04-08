######################
# FIT META D' TO DATA - BASED ON SAMPLING BIAS
#######################

### CLEAR WORKSPACE ################################################
#rm(list=ls())

### PRE-REQ ########################################################

# Import Packages 
source("f_essentials.R")
source("functions_metaDprime.R")

# Load my.data (if it is not already present)
if (!exists("my.data.a")){
  load("!Data/data_et.RData")
  print("loading my.data")
}

### PRE-ALLOCATION ################################################
# load participants' IDs
pID <- unique(my.data.a$participant)

m.meta <- matrix(NA, ncol = 10, nrow = length(pID))
m.meta2 <- matrix(NA, ncol = 5, nrow = length(pID))
### CALCULALTE META-D' and D' #####################################
run.v <- c(1:length(pID)) # used to exclude participants

for (iparticipant in run.v){
  for (iTime in 1:5){
    f.data <- 
      my.data.a %>% 
      mutate(bin_time=ntile(time,5)) %>% 
      filter(participant==pID[iparticipant]) %>% 
      filter(my.data.a$bin_time==iTime)
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta[iparticipant,iTime] <- mean(as.numeric(model$meta_d))
    m.meta[iparticipant,iTime+5] <- mean(as.numeric(model$d1))
  }
}

### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
meta2 <- data.frame(m.meta2)
colnames(meta2)<-c("time1","time2","time3","time4", "time5") #,"time4", "time5"

# add a column with participant info
meta2$participant<-pID

# put all biases into one colums
meta.time<-
  meta2 %>% 
  gather(time, meta_D, time1, time2, time3) #, time4, time5

### PLOT DATA #####################################################

# META-D'
p.meta = ggplot(aes(x=as.factor(time), y=meta_D),data=meta.time) + 
  geom_bar(stat = "identity") + facet_wrap(~participant)+
  xlab("condition") + ylab("meta-d") + 
  geom_abline(intercept = 0, slope = 0)

