######################
# FIT META D' TO DATA 
#######################

### CLEAR WORKSPACE ################################################
#rm(list=ls())

### PRE-REQ ########################################################

# Import Packages 
source("functions_essentials.R")
source("functions_metaDprime.R")

# Load my.data (if it is not already present)
if (!exists("my.data.e")){
  load("!Data/data_et.RData")
  print("loading my.data")
}

### PRE-ALLOCATION ################################################
# load participants' IDs
pID <- unique(my.data$participant)

m.meta <- matrix(NA, ncol = 2, nrow = length(pID))

### CALCULALTE META-D' and D' #####################################
run.v <- c(1:length(pID)) # used to exclude participants

for (iparticipant in run.v){
  f.data <- 
    my.data.e %>% 
    filter(participant==pID[iparticipant]) 
  model <-
    DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,1] <- mean(as.numeric(model$meta_d))
  m.meta[iparticipant,2] <- mean(as.numeric(model$d1))
}

### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
meta <- data.frame(m.meta)
colnames(meta)<-c("meta","d")

# create a new matrix with metacognitive efficiency (meta-d' - d')
meta$diff <- meta$meta - meta$d
meta$diff2 <- meta$meta/meta$d

# add a column with participant info
meta$participant<-pID

### PLOT DATA #####################################################

# META-D'
p.meta = ggplot(aes(x=as.factor(participant), y=meta),data=meta) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("meta-d") + 
  geom_abline(intercept = 0, slope = 0)

# D'
p.d = ggplot(aes(x=as.factor(participant), y=d, fill=d),data=meta) + 
  geom_bar(stat = "identity") +
  xlab("condition") + ylab("dPrime") + 
  geom_abline(intercept = 0, slope = 0)

# D'-DIFF - metacognitive efficiency (meta-d' - d')
p.diff = ggplot(aes(x=as.factor(participant), y=diff2, fill=diff2),data=meta) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("efficiency")+ 
  geom_abline(intercept = 0, slope = 0)

s.meta.diff<-
  meta %>% 
  summarise_each (funs(mean,sd,sem), diff2)

# change the structure of the d.frame so that I can plot multiple columns
meta.p<-melt(meta,id.vars="participant")

### PUT ALL VALUES INTO A TABLE AND SAVE IT #######################

my.data.m<-merge(my.data.e,select(meta,c(participant,meta,d,diff,diff2)),by = "participant", all = TRUE)

save(meta,file='!Data/meta_values.RData') 
save(my.data.m,file='!Data/data_etMeta.RData') 
