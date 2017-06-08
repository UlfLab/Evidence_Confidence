##############################################################
### Functions and libraries needed for majority of the scripts
##############################################################


#essential libraries
library(tidyverse)
library(data.table)
library(dtplyr)
library(stringr)
library(jsonlite)
library(bit64)


# essential fuctions
get.dropbox.folder <- function() {
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  return(file_content)
}


