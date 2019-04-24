### EXCLUSION CRITERIA AND NUMBER OF EXCLUDED TRIALS BY PARTICIPANT ########################


exclusion <- function (my.data) {
  s.trials<-
    my.data %>% 
    group_by(partSes_ID) %>%
    summarise(N=n())
  
  s.et<-
    my.data[!with(my.data,is.na(dwell_right)& is.na(dwell_fix)&is.na(dwell_left)),] %>% 
    group_by(partSes_ID) %>%
    summarise(N=n())
  
  s.sample<-
    my.data %>% 
    filter(!is.na(dt_diff)) %>%
    group_by(partSes_ID) %>%
    summarise(N=n())
  
  s.noResp<-
    my.data %>% 
   filter(response.keys!="None") %>%
    filter(!is.na(dt_diff)) %>%
    group_by(partSes_ID) %>%
    summarise(N=n())
  
  s.conf<-
    my.data %>%
    filter(conf>0.1) %>%
    filter(!is.na(dt_diff)) %>%
    group_by(partSes_ID) %>%
    summarise(N=n())
  
  s.exclusion<-
    s.trials %>% 
    mutate(et=s.trials$N-s.et$N,
           sample=s.et$N-s.sample$N,
           noResp=s.sample$N-s.noResp$N,
           conf=s.noResp$N-s.conf$N,
           all=s.trials$N-s.conf$N,
           final = N -all)
           

  
  return(s.exclusion)
}