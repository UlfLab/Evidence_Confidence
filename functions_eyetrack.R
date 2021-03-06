###########################################
#EYE TRACKER FUNCTIONS
###########################################

### FIND TRIAL START/END TIMES AND REMOVE THOSE LINES FROM THE FILE ################
clean_eye_tracker_data<-function(id){
  
  # First read in whole text file
  text_file<-scan(files.eye[id],character(0),sep="\n")
  
  # Expressions to look for - trial start, trial end 
  # and timestamp number with 10 to 11 digits at the start of each line
  regexp<-"Trial Started"
  regexp2<-"Trial Ended"
  regexp3<-"^[[:digit:]]{10,11}" 
  
  # Where does the trial start/end?
  start_trial_lines<-which(str_detect(text_file,regexp))
  end_trial_lines<-which(str_detect(text_file,regexp2))
  
  # when did trials start?
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
 
  #check that all 240 trials have been assigned and remove the helpers
  #unique(helper2)
  rm(helper,helper2)
  
  #remove all eye tracking not belonging to particular trials
  # and data points from when the participant was not looking at the screen
  eye.data<-
    eye.data %>% 
    filter(trial>0) %>% 
    filter(V2>0)%>% 
    filter(V8>0)
 return (eye.data)
}

### ADD THE START/END OF EYE FIXATIONS #############################################
 
# IDENTIFY FIXATION POINTS
    # The screen has reslutoin of 1680x1050
    # the positions & sizes of grates are given by height of the screen (1050)
    #   
    #   -> size 0.4*height across
    # THe fixation cross 
    #   ->positioned at the centre of the screen at X=840 and Y= 525
    #   ->lenght: 0.2*(1680/2) = 168 (+17 margin) , height: 0.1*1050 = 105 (+11 margin)
    #   -> FIXATION CROSS: x = 748 - 933
    
    # THe grates 
    #  -> positioned 0.5*height from the centre of the screen
    #  -> Y= 525, LEFT: X=840-525=315px, RIGHT: X=840+525=1365px
    #  -> have a diameter of 0.4*1050=420 (+ 42px = 10% margin) 
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
  
  #see if this calculation is correct
  #ggplot(aes(x=V2,fill=fix),data=subset(eye.data))+geom_histogram()
  
  #calculate the time spent in the left/right/fixed area per trial
  
  #indicate changes 
  eye.data$change<-0
  
  helper<-c("start",eye.data$fix[1:nrow(eye.data)-1])
  helper2<-c(eye.data$fix[2:nrow(eye.data)],NA)
  
  # mark the start of fixation as 1 - the current data point is different from the previous one
  eye.data$change<-ifelse(eye.data$fix==helper,0,1)
  
  # mark the end of fixation as 2 - the current data point is different from the following one
  eye.data$change<-ifelse(eye.data$fix==helper2,eye.data$change,2)
  
  # cleanup
  rm(helper,helper2)
  
  #exclude tracks with length 1, those trials have eye.data$change==2 that 
  # is immediately preceeded by another 2 (two ends of fixation in the row)
  # if that happens exclude the second one (as it is not preceded by a start of fixation)
  
  helper<-c(NA,eye.data$change[1:nrow(eye.data)-1])
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==2),]
  rm(helper)
  
  #last cell must be 2
  eye.data$change[nrow(eye.data)]<-2
  
  
  ###### CHECK THAT EVERY TRIAL STARTS WITH change==1 AND ENDS WITH change==2
  
  helper2<-eye.data$change
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)){
    trial.data<-
      eye.data %>%
      select(V1,trial,change) %>%
      filter(trial==itrial)
    start.trial = min(trial.data$V1)
    end.trial = max(trial.data$V1)
    
    helper2[helper==start.trial]<-1
    helper2[helper==end.trial]<-2
  }
  
  # add to the main file
  eye.data$change<-helper2
  
  # clear up  
  rm(helper,helper2)
  
  ### AND REMOVE TRACKS WITH LENGTH 1 (that pop up again),
  
  # if two 2s are next to each other - delete the second
  helper<-c(NA,eye.data$change[1:nrow(eye.data)-1])
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==2),]
  rm(helper)
  
  # if two 1s are next to each otehr - delete the first
  helper<-c(eye.data$change[2:nrow(eye.data)],NA)
  eye.data<-eye.data[!(eye.data$change==helper&eye.data$change==1),]
  rm(helper)
  
  #last cell must be 2(end of fixation) - should be true anyway but better safe than sorry
  eye.data$change[nrow(eye.data)]<-2
 
  ### LABEL THE FIRST AND LAST MEANINGFUL(LEFT/RIGHT) FIXATION OF THE TRIAL
  eye.data$fixPos<-"none"
  
  helper2<-eye.data$fixPos
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)){
    trial.data<-
      eye.data %>%
      select(V1,V4,trial,change,fix) %>%
      filter(trial==itrial&fix%in%c("left","right"))
    start.fix = trial.data$V1[which(trial.data$change==1)]
    end.fix = trial.data$V1[which(trial.data$change==2)]
    
    if (length(start.fix)==0){next}
    
    helper2[helper>=start.fix[1]&helper<=end.fix[1]]<-"firstFix"
    helper2[helper>=start.fix[length(start.fix)]&helper<=end.fix[length(start.fix)]]<-"lastFix"
  }
  
  # add to the main file
  eye.data$fixPos<-helper2
  
  # clear up  
  rm(helper,helper2)
  
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
  
   names(ec.final)<-c("fix","trial","duration")
   ec.final$duration2<-as.integer(ec.final$duration)
  
  #now get last fixation durations
  ec.fl<-
    ec.data %>% 
    select(fix,trial,duration) %>%
    dcast(fix+trial~.,fun=function(x) tail(x,n=1),value.var="duration",fill=0) 
  names(ec.fl)<-c("fix","trial","last_fix")
  ec.fl$last_fix<-as.integer(ec.fl$last_fix)
  
  #and first fixation duration
  ec.ff<-
    ec.data %>% 
    select(fix,trial,duration) %>%
    dcast(fix+trial~.,fun=function(x) head(x,n=1),value.var="duration",fill=0) 
  names(ec.ff)<-c("fix","trial","first_fix")
  ec.ff$first_fix<-as.integer(ec.ff$first_fix)
  

  #calculate right of total time for left and right fixation and also their ratio
  ec.wide<-
    ec.final %>% 
    select(fix,trial,duration2) %>% 
    spread(fix,duration2) %>% 
    mutate(ratio=right/(left+right))
  
  colnames(ec.wide)<-c("trial","duration_fix","duration_left","duration_none","duration_right","ratio")
  
  #make transformation for last and first fixation durations
  ec.wide.last.dur<-
    ec.fl %>% 
    select(fix,trial,last_fix) %>% 
    spread(fix,last_fix)
  ec.wide.first.dur<-
    ec.ff %>% 
    select(fix,trial,first_fix) %>% 
    spread(fix,first_fix)
  
  ### DETERMINE FIRST AND LAST FIXATION IN THE TRIAL
  fix.first<-
    eye.data %>%
    select(fix,trial,change) %>% 
    filter(fix%in%c("left","right")&change==1) %>% 
    group_by(trial) %>% 
    summarise(first_fix=head(fix,n=1), # returns the first data.point in the fix column
              last_fix=tail(fix,n=1), # returns the last data.point in the fix column
              changes=length(fix))
  
  
  # sometimes fix first looses a trial
  # this adds the lost trials to the data frame with NA as values
  c <- 240
  # c <- ifelse (any(unique(eye.data$trial)>160),240,ifelse(any(unique(eye.data$trial)>120),160,120))
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
  
  output <- merge(fix.first,ec.wide,by="trial",all=TRUE)
  
   return(output)
}
  
### NUMBER THE FIXATION WITHIN TRIAL #############################################
eye_tracker_trialFixation<-function(eye.data){
  
  # NUMBER THE FIXATIONS and ADD TIMINGS TO THEM
  eye.data$fixNum <-0
  eye.data$time <- 0
  
  helper3<-eye.data$time
  helper2<-eye.data$fixNum
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)){
    trial.data<-
      eye.data %>%
      select(V1,V4,trial,change,fix) %>%
      filter(trial==itrial)
    fix.num.s = trial.data$V1[which(trial.data$change==1)]
    fix.num.e = trial.data$V1[which(trial.data$change==2)]
    
    for(ifix in 1:length(fix.num.s)){
      helper2[helper>=fix.num.s[ifix]&helper<=fix.num.e[ifix]]<-ifix
      
      # must be smaller number (or float?) to work, otherwise the conversion from numeric to vector comes up with weird values
      helper3[helper>=fix.num.s[ifix]&helper<=fix.num.e[ifix]]<-(helper[helper>=fix.num.s[ifix]&helper<=fix.num.e[ifix]]-helper[helper==fix.num.e[ifix]])/1000
    }
  }
  
  # add to the main file
  eye.data$fixNum<-helper2
  eye.data$time <- helper3
  
  # clear up  
  rm(helper,helper2,helper3)
  
  return(eye.data)
}
  
### ADD TIMINGS TO FIXATIONS #####################################################
  eye_tracker_trialTiming<-function(eye.data){  
  
  # ADD TIMINGS TO TRIALS
  
  eye.data$timeTrial <- NA
  eye.data$startTrial <- 0
  
  helper3<- eye.data$startTrial
  helper2<-eye.data$timeTrial
  helper<-eye.data$V1
  
  for(itrial in unique(eye.data$trial)[unique(eye.data$trial)!=0]){
    trial.data<-
      eye.data %>%
      select(V1,trial) %>%
      filter(trial==itrial)
    start.trial = min(trial.data$V1)
    end.trial = max(trial.data$V1)
    
    helper2[helper>=start.trial&helper<=end.trial]<-(helper[helper>=start.trial&helper<=end.trial]-helper[helper==end.trial])/1000
    helper3[helper==start.trial]<-1
}


  # add to the main file
  eye.data$timeTrial <- helper2
  eye.data$startTrial <- helper3
  
  # clear up  
  rm(helper,helper2,helper3)
  
  return(eye.data)
  }


