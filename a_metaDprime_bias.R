######################
# FIT META D' TO DATA - BASED ON SAMPLING BIAS
#######################

### CLEAR WORKSPACE ################################################
#rm(list=ls())

### PRE-REQ ########################################################

# Import Packages 
source("functions_essentials.R")
source("functions_metaDprime.R")

# Load my.data (if it is not already present)
if (!exists("my.data.a")){
  load("!Data/data_et.RData")
  print("loading my.data")
}

### PRE-ALLOCATION ################################################
# load participants' IDs
pID <- unique(my.data.a$participant)

m.meta <- matrix(NA, ncol = 3, nrow = length(pID))
m.meta2 <- matrix(NA, ncol = 5, nrow = length(pID))
### CALCULALTE META-D' and D' #####################################
run.v <- c(1:length(pID)) # used to exclude participants

for (iparticipant in run.v){
  for (iBias in 1:3){
  f.data <- 
    my.data.a %>% 
    filter(participant==pID[iparticipant]) %>% 
    filter(my.data.a$bin_bias_ch==iBias)
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,iBias] <- mean(as.numeric(model$meta_d))
   }
}

### CONVERT THE MATRIX INTO A DATA FRAME ANd CALCULATE D'DIFF #####

#convert matrices into data.frames (for ggplot, and easier handling)
meta <- data.frame(m.meta)
colnames(meta)<-c("bias1","bias2","bias3") #,"bias4","bias5"

# add a column with participant info
meta$participant<-pID

# put all biases into one colums
meta2<-
  meta %>% 
  gather(bias, meta_D, bias1, bias2, bias3) #, bias4, bias5

### PLOT DATA #####################################################

# META-D'
p.meta = ggplot(aes(x=as.factor(bias), y=meta_D),data=meta2) + 
  geom_bar(stat = "identity") + facet_wrap(~participant)+
  xlab("condition") + ylab("meta-d") + 
  geom_abline(intercept = 0, slope = 0)

### AVERAGE RESULTS ##########
meta2_agg<-
  meta2 %>%
  group_by(bias) %>%
  summarise_at(vars(meta_D),funs(mean,sd,sem,ymin,ymax))

#graph - barplot
ggplot(aes(y=mean,x=as.factor(bias)),data=meta2_agg)+
  geom_bar(stat="identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.1) + 
  ylab("meta_D") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\metaD_bias_ch_bar.jpg", width = 15, height = 15, units = "cm")
  
#graph - boxplot
ggplot(aes(y=meta_D,x=as.factor(bias)),data=meta2)+
  geom_boxplot() + 
  ylab("meta_D") + xlab("CHOSEN - sampling bias") + theme_classic()
ggsave(filename="!Analysis\\metaD_bias_ch_boxplot.jpg", width = 15, height = 15, units = "cm")


# test curvilinear relationship
meta3<-meta2
meta3$biasNum<-as.factor(meta3$bias)
levels(meta3$biasNum)<-c(1:5)
meta3$biasNum<-as.numeric(meta3$biasNum)
meta3$participant2<-substring(meta3$participant,1,3)


fit.meta<-lmer(meta_D~poly(biasNum,2)+(1|participant),meta3)

plot(fitted(fit.meta),residuals(fit.meta))

sjp.lmer(fit.meta, 
         type = "fe", 
         sort = TRUE,
         title = "",
         p.kr = FALSE,
         pred.labels=labels("bias_lin","bias_sq"))
ggsave(filename="!Analysis\\metaD_regression.jpg", width = 15, height = 15, units = "cm")



### PUT ALL VALUES INTO A TABLE AND SAVE IT #######################

my.data.m<-merge(my.data.e,select(meta,c(participant,meta,d,diff,diff2)),by = "participant", all = TRUE)

save(meta,file='!Data/meta_values.RData') 
save(my.data.m,file='!Data/data_etMeta.RData') 
