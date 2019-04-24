
### FUNCTIONS FOR META-D' CALCULATIONS #####################################


DataMetaD<- function (filtered.data, nBins=3) {
  filtered.data$bin_zConf<-ntile(filtered.data$zConf,nBins)
  
  nR_S1<-
    filtered.data %>% 
    group_by(response.corr,bin_zConf)%>%
    filter(response.keys=="left")%>%
    summarise(N=n())%>%
    spread(response.corr,value=N,drop=F,fill=0)
  
  a<-as.numeric(nR_S1$`1`)[nBins:1]
  b<-as.numeric(nR_S1$`0`)
  nR_S1<-c(a,b)
  nR_S1[is.na(nR_S1)]<-0
  
  nR_S2<-
    filtered.data %>% 
    group_by(response.corr,bin_zConf)%>%
    filter(response.keys=="right")%>%
    summarise(N=n())%>%
    spread(response.corr,value=N,drop=F,fill=0)
  
  a<-as.numeric(nR_S2$`0`)[nBins:1]
  b<-as.numeric(nR_S2$`1`)
  nR_S2<-c(a,b)
  nR_S2[is.na(nR_S2)]<-0
  
  return(c(nR_S1,nR_S2))
}


FitMetaD <- function (count) {
  require(rjags)
  require(coda)
  
  nTot <- sum(count) #do we need this?
  nRating <- length(count)/4 # finds out nBins value
  
  #How does this work?
  forJags <- list(counts=count,nratings=nRating,Tol=0.0001)
  #
  mod.1<-jags.model(file="Bayes_metad2.txt",data = forJags,n.chains=4)
  update(mod.1, 1000)
  mod.1.samp<-jags.samples(mod.1,
                           c('meta_d','d1','c1','cS1','cS2'),
                           1000)
  return(mod.1.samp)
}