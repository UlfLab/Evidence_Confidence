###########################################
#EYE TRACKER FUNCTIONS
###########################################

### FIND TRIAL START/END TIMES AND REMOVE THOSE LINES FROM THE FILE ################
clean_eye_tracker_data<-function(id){
  
  # First read in whole text file
  text_file<-scan(files.eye[id],character(0),sep="\n")
  
  # Expressions to look for - trial start, trial end 
  # and timestamp number with 10 to 11 digits at the start of each line
  regexp<-regex("Trial Started",ignore_case=T) 
  regexp2<-regex("Trial Ended",ignore_case=T)
  regexp3<-"^[[:digit:]]{9,11}" 
  
  # Where does the trial start/end?
  start_trial_lines<-which(str_detect(text_file,regexp))
  end_trial_lines<-which(str_detect(text_file,regexp2))
  
  trial_start_times<<-as.numeric(str_extract(text_file[start_trial_lines],pattern=regexp3))
  trial_end_times<<-as.numeric(str_extract(text_file[end_trial_lines],pattern=regexp3))
  
  # Save the cleaned data
  cat(text_file[-c(start_trial_lines,end_trial_lines)],file="file.txt",sep="\n")
 
  #added so that I can use this file in MATLAB
  #cat(text_file[-c(start_trial_lines,end_trial_lines)],file=paste0(eye.participant[id],"_file.txt"),sep="\n")
  rm(text_file)
}

### INSERT TRIAL NUMBERS ###########################################################
trial_eye_track_data<-function(){
 
  # Read in cleaned file (from the previous function)
  eye.data<-fread("file.txt")
  
  ## INSERT TRIAL NUMBERS
  eye.data$trial<-0
  helper2<-eye.data$trial
  helper<-eye.data$V1
  
  # look up the data points that occured between each start and end of the trial
  # and give them a trial number starting with #1 
  for (i in 1:length(trial_start_times)){
    helper2[helper>=trial_start_times[i]&helper<=trial_end_times[i]]<-i}
  
  eye.data$trial<-helper2
  rm(helper,helper2)
  
  #remove all eye tracking not belonging to particular trials
  # and data points from when the participant was not looking at the screen
  eye.data<-
    eye.data %>% 
    filter(trial>0) %>% 
    filter(V2>0)%>% 
    filter(V8>0) %>% 
    mutate(avg=(V2+V8)/2)
                        
 return (eye.data)
}

### ADD THE START/END OF EYE FIXATIONS #############################################
 
# IDENTIFY FIXATION POINTS
    # The screen has reslutoin of 1680x1050
    # the positions & sizes of grates are given by width of the screen (1050)
    # -> size 0.4*width
    
    # Fixation cross 
    #   ->positioned at the centre of the screen at X=840 and Y= 525
    #   ->lenght: 0.2*(1680/2) = 168 (+17 (10%) margin) , height: 0.1*1050 = 105 (+11 margin)
    #   -> FIXATION CROSS: x = 748 - 933
    
    # Gratings
    #  -> positioned 0.5*width from the centre of the screen
    #  -> Y= 525, LEFT: X=840-525=315px, RIGHT: X=840+525=1365px
    #  -> Diameter of 0.4*1050=420 (+ 42px = 10% margin) 
    #  -> LEFT: x = 84 - 546px; RIGHT: x = 1134 - 1596


fixation_eye_track_data<-function(eye.data){

  # create vector with the x-axis boundaries for each position
  left.bounds<-c(84,546)
  right.bounds<-c(1134,1596)
  fixation.bounds<-c(748,933)
  
  eye.data$fix<-"none"
  eye.data$fix[(eye.data$V2-left.bounds[1])*(eye.data$V2-left.bounds[2])<0]<-c("left")
  eye.data$fix[(eye.data$V2-right.bounds[1])*(eye.data$V2-right.bounds[2])<0]<-c("right")
  eye.data$fix[(eye.data$V2-fixation.bounds[1])*(eye.data$V2-fixation.bounds[2])<0]<-c("fix")
  
  #calculate the time spent in the left/right/fixed area per trial
  
  # remove fixations with length 1
  eye.data<-eye.data[!(eye.data$fix!=lag(eye.data$fix,1)&eye.data$fix!=lead(eye.data$fix,1)),]
  #eye.data$f1<-eye.data$fix!=lag(eye.data$fix,1)&eye.data$fix!=lead(eye.data$fix,1)
  
  #indicate changes in fixations 
  eye.data$change<-0

  # mark the start of fixation as 1 - the current data point is different from the previous one
  eye.data$change<-ifelse(eye.data$fix==lag(eye.data$fix,1),0,1)
  
  # mark the end of fixation as 2 - the current data point is different from the following one
  eye.data$change<-ifelse(eye.data$fix==lead(eye.data$fix,1),eye.data$change,2)
  
  #first cell must be 1 and last cell must be 2
  eye.data$change[1]<-1
  eye.data$change[nrow(eye.data)]<-2
  
  ###### CHECK THAT EVERY TRIAL STARTS WITH change==1 AND ENDS WITH change==2
  
  eye.data<-
    eye.data %>% 
    group_by(trial) %>% 
    mutate(change=ifelse(head(V1,n=1)==V1,1,ifelse(tail(V1,n=1)==V1,2,change)))
  
  ### AND REMOVE TRACKS WITH LENGTH 1 (that pop up again)
  eye.data<-eye.data[!(eye.data$change==lag(eye.data$change,1)&eye.data$change==2),]
  eye.data<-eye.data[!(eye.data$change==lead(eye.data$change,1)&eye.data$change==1),]
 
    return(eye.data)
}

### CALCULATE THE NUMBER OF FIXATIONS, THEIR POSITION & DURATION #################
calc_eye_tracker_values<-function(eye.data){
  
  ### CALCULTE LENGTH OF FIXATION
  # Extract the start/end of each fixation
  eye.change.data<-
    eye.data %>% 
    select(V1,fix,change,trial) %>% 
    filter(change>0)
  
  # put in the same row data for start of fixation(V1) with the timestamp of its end (time.end)
  # and calculate their difference to get the fixation time
  ec.data<-eye.change.data[eye.change.data$change==1,]
  ec.data$time.end<-eye.change.data$V1[eye.change.data$change==2]
  ec.data$duration<-ec.data$time.end-ec.data$V1
  
   # for each combination of fix & trial, sum up all values of duration
   ec.final<-
    ec.data %>% 
    select(fix,trial,duration) %>% 
    dcast(fix+trial~.,fun=sum) 
  
   names(ec.final)<-c("fix","trial","dwell")
   ec.final$dwell<-as.integer(ec.final$dwell)
  
  #calculate right of total time for left and right fixation and also their ratio
  ec.wide<-
    ec.final %>% 
    select(fix,trial,dwell) %>% 
    spread(fix,dwell)
  
  colnames(ec.wide)<-c("trial","dwell_fix","dwell_left","dwell_none","dwell_right")
  
  ### DETERMINE FIRST AND LAST FIXATION IN THE TRIAL AND NUM OF LEFT/RIGHT FIXATIONS 
  # If there are two fixations to the right(left) in the row, count them as 1
  # i.e. fixation time stays the same as those two were separate fixations (no none fix time added)

  fix.first<-
    ec.data %>%
    select(fix,trial,change) %>% 
    filter(fix%in%c("left","right")&change==1) %>% 
    mutate(double = ifelse(fix==lag(fix)&trial==lag(trial),1,0)) %>% 
    group_by(trial) %>% 
    summarise(first_fix=head(fix,n=1), # returns the first data.point in the fix column
              last_fix=tail(fix,n=1), # returns the last data.point in the fix column
              nFix1=length(fix),
              nFix2 = length(fix)-sum(double)) %>% 
    mutate(nFix = ifelse(is.na(nFix2)==1,nFix1,nFix2))
  
  
  # sometimes fix first looses a trial
  # this adds the lost trials to the data frame with NA as values
  c <- 240
  a<-seq(1,c)
  
  add<-a[!a%in%fix.first$trial]
  
  if (length(add)>0){
    for (i in add){
      k<-fix.first[1,]
      k[1,]<-NA
      k$trial<-i
      fix.first<-bind_rows(fix.first,k)
      fix.first<-fix.first[order(fix.first$trial),]
    }
  }
  
  # ADD FIXATION TIMES and FIRST/LAST FIXATION TOGETHER
  
  output <- merge(subset(fix.first,select=-c(nFix1,nFix2)),ec.wide,by="trial",all=TRUE)
  
   return(output)
}
 

### FIXATION LENGTH of EACH FIXATION IN TRIAL ###############

calc_length_IndFix<-function(eye.data){
  
  ### Select only left and right fixations and number them
  # if there are two fixations to the same location in a trial in the row -> merge them
  eye.data.lr<-
    as.data.frame(eye.data) %>% 
    filter(fix%in%c("left","right")) %>% 
    filter(change!=0) %>% 
    subset(select=c(V1,trial,change,fix)) %>% 
    group_by(trial,change) %>% 
    mutate(delFix=ifelse(change==1&fix==lag(fix),1,ifelse(change==2&fix==lead(fix),1,0))) %>% 
    mutate(delFix=ifelse(is.na(delFix),0,delFix)) %>% 
    subset(delFix!=1,select=-c(delFix)) %>% 
    group_by(trial,change) %>% 
    mutate(fixNum=seq_along(V1))
  
  ### CALCULTE LENGTH OF FIXATION
  # put in the same row data for start of fixation(V1) with the timestamp of its end (time.end)
  # and calculate their difference to get the fixation time
  ec.data<-
    as.data.frame(eye.data.lr) %>% 
    mutate(change2=ifelse(change==1,"start","end"),
           time = as.numeric(V1)/1) %>% 
    subset(select=-c(change,V1)) %>% 
    spread (change2,time) %>% 
    mutate(duration = end - start) %>% 
    subset(select=-c(end,start))
  
  
  return(ec.data)
} 
