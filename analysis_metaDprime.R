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
if (!exists("my.data")){
  load("!Data/ce_behavioural.RData")
  print("loading my.data")
}

### PRE-ALLOCATION ################################################
# load participants' IDs
pID <- unique(my.data$participant)


m.meta <- matrix(NA, ncol = 1, nrow = length(pID))
m.d <- matrix(NA, ncol = 1, nrow = length(pID))

### CALCULALTE META-D' and D' #####################################
# remove data points from trials where participants failed to respond or released the response button prematurely

my.data.a<-
  my.data %>% 
  filter(!is.na(zConf)&key_resp_direction.keys!="None")%>%
  filter(conf>0.1)%>%
  
run.v <- c(1:length(pID)) # used to exclude participants

for (iparticipant in run.v){
  f.data <- 
    my.data.a %>% 
    filter(participant==pID[iparticipant]) 
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,1] <- mean(as.numeric(model$meta_d))
  m.d[iparticipant,1] <- mean(as.numeric(model$d1))
}

### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
m.meta <- data.frame(m.meta)
m.d <- data.frame(m.d)

# create a new matrix with metacognitive efficiency (meta-d' - d')
m.diff <- m.meta - m.d

# add a column with participant info
m.meta$participant<-pID
m.d$participant<-pID
m.diff$participant<-pID

### PLOT DATA #####################################################

# PLOT by participant
# change the structure of the d.frame so that I can plot multiple columns
m.meta.p<-melt(m.meta,id.vars="participant")
m.d.p <-melt(m.d,id.vars="participant")
m.diff.p <-melt(m.diff,id.vars="participant")

# META-D'
p.meta = ggplot(aes(x=variable, y=value, fill=variable),data=m.meta.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("meta-d") + 
  geom_abline(intercept = 0, slope = 0)

f.meta <- group_by(m.meta.p,participant) %>%
  do(meta = p.meta %+% ., 
     meta_F = p.meta + facet_wrap(~ participant))

# D'
p.d = ggplot(aes(x=variable, y=value, fill=variable),data=m.d.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("dPrime") + 
  geom_abline(intercept = 0, slope = 0)

f.d <- group_by(m.d.p,participant) %>%
  do(d = p.d %+% .,
     d_F = p.d + facet_wrap(~ participant))

# D'-DIFF - metacognitive efficiency (meta-d' - d')
p.diff = ggplot(aes(x=variable, y=value, fill=variable),data=m.diff.p) + 
  geom_bar(stat = "identity") + 
  xlab("condition") + ylab("metaD-dPrime")+ 
  geom_abline(intercept = 0, slope = 0)

f.diff <- group_by(m.diff.p,participant) %>%
  do(diff = p.diff %+% ., 
     diff_F = p.diff + facet_wrap(~ participant))

### PUT ALL PLOTS/VALUES INTO A SINGLE TABLE AND SAVE IT #######################

## SAVE PLOTS
# create a list with all data.frames to be merged
f.all <- list(f.meta,f.d,f.diff)

# use reduce to call function merge (merges two data.frames)
meta_plots <- Reduce(function(...) merge(...,by = "participant", all = TRUE), f.all)
save(meta_plots,file='!Data/meta_plots.RData')

### SAVE DATA FILES
m.meta$type<-"meta"
m.d$type<-"dprime"
m.diff$type<-"diff"

m.combined <-rbind(m.meta,m.d,m.diff)
save(m.combined,file='!Data/meta_values.RData') 
