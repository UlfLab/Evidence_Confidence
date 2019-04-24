### PREPARE MY DATA FOR ANALYSIS ############
# adds additional columns needed for the analysis scripts

# needs my.data file with behavioural and eye.tracking data 
# here done after exclusion


### PREPARE DATA FILE #########################################################
my.data.a<- 
  my.data.e %>%
  mutate(bias_r=dwell_right/(dwell_left+dwell_right),
         bias_l=dwell_left/(dwell_left+dwell_right),
         bias_ch=ifelse(response.keys=="left",bias_l,bias_r),
         bias_1st = ifelse(first_fix=="left",bias_l,bias_r),
         bias_cor = ifelse(response.corr==1,bias_ch,1-bias_ch),
         time = (dwell_left+dwell_right)/1000000) %>% 
  mutate(nFix_odd = nFix%%2,
         choice_r=ifelse(response.keys=="right",1,0),
         choice_l=ifelse(response.keys=="left",1,0),
         first_r=ifelse(first_fix=="right",1,0),
         last_r=ifelse(last_fix=="right",1,0),
         first_l=ifelse(first_fix=="left",1,0),
         last_l=ifelse(last_fix=="left",1,0),
         first_ch=ifelse(first_fix==response.keys,1,0),
         last_ch=ifelse(last_fix==response.keys,1,0),
         first_cor=ifelse(first_fix==correct,1,0),
         last_cor=ifelse(last_fix==correct,1,0)) %>% 
  mutate(group_ch = factor(first_ch+2*last_ch, labels = c("00","F0","0L","FL")),
         group_l = factor(first_l+2*last_l, labels = c("00","F0","0L","FL")),
         group_cor = ifelse(correct=="left",factor(first_l+2*last_l, labels = c("00","F0","0L","FL")),
                            factor(first_r+2*last_r, labels = c("00","F0","0L","FL")))) %>% 
  group_by(partSes_ID) %>% 
  mutate(bin_conf = ntile(conf,3),
         bin_bias_r = ntile(bias_r,5),
         bin_bias_l = ntile(bias_l,5),
         bin_bias_ch = ntile(bias_ch,5),
         bin_time = ntile(time,5),
         zConf = scale(conf),
         zRT = scale(response.rt),
         zBias_r=scale(bias_r,center=0.5),
         zBias_l=scale(bias_l,center=0.5),
         zBias_ch=scale(bias_ch,center=0.5),
         zBias_1st = scale(bias_1st,center=0.5),
         zBias_cor = scale(bias_cor,center=0.5),
         zTime = scale(time))


### SAVE ##############
save(my.data.a,file='_Data\\d_mainTask_btea.RData')



