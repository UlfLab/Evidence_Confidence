sem<-function(id){

sem1 = sd(id)/ sqrt(length(id))
return(sem1)
}

ymin<-function(id){
  
  ymin = mean(id)-sem(id)
  return(ymin)
}

ymax<-function(id){
  
  ymax = mean(id)+sem(id)
  return(ymax)
}
