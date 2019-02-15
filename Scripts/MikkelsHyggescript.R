rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

library("data.table")

## Loading all data
data.path = "../Watts_DistrictHeatingData_2018/"
file.names <- dir(data.path, pattern =".csv")
n <- length(file.names)
Datalengths = c(1,n)
DATA <- vector(mode="list",length=n)

for(i in 1:n){
  df.temp <- read.csv(paste(data.path,file.names[i], sep = ""),sep=";", stringsAsFactors=FALSE, header = TRUE,dec=',')
  if(dim(df.temp)[2]==1){
    df.temp <- read.csv(paste(data.path,file.names[i], sep = ""),sep="\t", stringsAsFactors=FALSE, header = TRUE,dec=',')
    df.temp$StartDateTime <- strptime(df.temp$StartDateTime, format = "%d/%m/%Y %H.%M", tz = "GMT")
    df.temp$EndDateTime <- strptime(df.temp$EndDateTime, format = "%d/%m/%Y %H.%M", tz = "GMT")
  }else{
    names(df.temp)[1] = 'StartDateTime'
    df.temp$X <- NULL
    df.temp$StartDateTime <- strptime(df.temp$StartDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
    df.temp$EndDateTime <- strptime(df.temp$EndDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  }
  
  DATA[[i]] <- df.temp
  Datalengths[i] = length(df.temp)
}