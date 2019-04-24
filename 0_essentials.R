##############################################################
### Functions, libraries and paths needed the scripts
##############################################################

#libraries
library(tidyverse)
library(data.table)
library(dtplyr)
library(stringr)
library(jsonlite)
library(bit64)
library(afex)
library(sjPlot)
library(sjlabelled)
library(gridExtra)
library(coda)
library(rjags)
library(optimx)


# fuctions
source("f_sem.R")
source("f_exclusion.R")
source("f_eyetrack.R")
source("f_metaD.R")

# path to the data
data.folder<-"C:\\Users\\Majda\\Documents\\Thesis\\Data_cleanedUp"



#install.packages(c("tidyverse","data.table","dtplyr","stringr","jsonlite","bit64","afex","sjPlot","sjlabelled","gridExtra","coda","rjags"))
#install.packages(c("Matrix","grid","stats","utils","methods","stringi","glue","snakecase"))
#install.packages(c("MASS","broom"))
#install.packages(c("modelr"))
