setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if(!exists("n")){
  source("data.R")
}

Summer<-c(as.POSIXct(as.Date('2018-05-15'),format = "%Y-%m-%d", tz = "GMT"),as.POSIXct(as.Date('2018-09-15'),format = "%Y-%m-%d", tz = "GMT"))

tmp<-data[[1]][data[[1]]$ObsTime>Summer[1],]
tmp<-tmp[tmp$ObsTime<Summer[2],]

tmpc<-tmp$Volume*tmp$CoolingDegree

hour(tmp$ObsTime)

plot(hour(tmp$ObsTime),tmpc)#,type='l')

     