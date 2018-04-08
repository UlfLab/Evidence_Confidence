### DATA FILE FOR HDDM ANALYSIS ##################################

# more likely to be correct if the chosen option was fixated for longer
my.hddm.acc <- 
  my.data.a %>% 
  select(participant,key_resp_direction.corr,key_resp_direction.rt,bias_ch)

colnames(my.hddm.acc)=c("participant"="subj_idx", "key_resp_direction.corr"="response","key_resp_direction.rt"="rt","bias_ch"="bias_ch")

write.csv(my.hddm.acc, file = "_Data/data_ddm_accuracy.csv")

# more likely to chose the right option if the right stimuli was fixated for longer
my.hddm.ch <- 
  my.data.a %>% 
  select(participant,ch_right,key_resp_direction.rt,first_r,key_resp_direction.corr)

colnames(my.hddm.ch)=c("participant"="subj_idx", "ch_right"="response","key_resp_direction.rt"="rt","first_r"="first_r","key_resp_direction.corr"="stim_col")

write.csv(my.hddm.ch, file = "_Data/data_ddm_choice.csv")
