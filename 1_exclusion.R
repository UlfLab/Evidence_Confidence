### EXCLUDE TRIALS AND COUNT EXCLUDED TRIALS BY PARTICIPANT #########################

### PRE-REQS ##################################################################

# requires my.data file that contains both eye.tracking and behavioural data

### COUNT AND PLOT EXCLUDED TRIALS ####
s.exclusion<-exclusion(my.data)


ggplot(aes(y=all,x=reorder(as.factor(partSes_ID),all)),data=s.exclusion) +geom_bar(stat='identity',position="dodge")

### REMOVE EXCLUDED TRIALS FROM MY.DATA #############################################

my.data.e<-
  my.data %>% 
  filter(conf>0.1) %>%
  filter(!is.na(dt_diff))

### REMOVE EXCLUDED SESSIONS FROM MY.DATA ##################
part.excl<-c(8872,8862,1152,1153,1756,6632)
# 1756 - accuracy < 55%   
# 8872,8862 - accuracy > 85%
# 1152,1153 - confidence below treshold -> more than 50% excluded 
# 6632 - Not enought eyetracking data  -> more than 50% trial excluded

my.data.e <-subset(my.data.e,!partSes_ID %in% part.excl)


### SAVE #############################################
save(my.data.e,file='_Data\\d_mainTask_bte.RData')




  





