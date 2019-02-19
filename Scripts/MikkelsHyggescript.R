#data.R
if(1==1){
  rm(list = ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
  
  library("data.table")
  
  # Loading all data
  data.path = "../Watts_DistrictHeatingData_2018/"
  file.names <- dir(data.path, pattern =".csv")
  n <- length(file.names)
  Datalengths = rep(c(1,n),nrow=n)
  data <- vector(mode="list", length = n)
  
  
  # Loading a single table to initialize dates
  dt.tmp <- read.table(paste(data.path,file.names[1], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
  names(dt.tmp)[1] = 'StartDateTime'
  StartDays <- strptime(dt.tmp$StartDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  EndDays <- strptime(dt.tmp$StartDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  
  for(i in 1:n){
    if (i == 5){
      dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
    }
    else
    {
      dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
      dt.tmp$X <- NULL
    }
    names(dt.tmp)[1] = 'StartDateTime'
    dt.tmp$StartDateTime <- strptime(dt.tmp$StartDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
    dt.tmp$EndDateTime <- strptime(dt.tmp$EndDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
    Datalengths[i] = length(dt.tmp)
    data[[i]] <- dt.tmp
    
    # Setting start and end times for each table.
    EndDays[i]= dt.tmp$StartDateTime[1]
    StartDays[i]=dt.tmp$StartDateTime[length(dt.tmp$StartDateTime)]
    
  }
  
  # Reading weather data  
  weather <- read.table('../WeatherData_01-01-2018_09-05-2019.csv', sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
  
  weather$StartDateTime = strptime(weather$StartDateTime,format='%d/%m/%Y %H.%M',tz = 'GMT')
  weather$IsHistoricalEstimated=weather$IsHistoricalEstimated=="True"
  
  # Sorting dates
  sStartDays <- StartDays[order(StartDays)]
  sEndDays <- EndDays[order(EndDays)]
  
  weatherStart = weather$StartDateTime[1]
  weatherEnd = weather$StartDateTime[length(weather$StartDateTime[weather$IsHistoricalEstimated==FALSE])]
  
  
  rm(i,n,file.names,data.path,dt.tmp,Datalengths)
}
#exploratory.R
if(2==3){
  rm(list = ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
  
  source("data.R")
  
}

starttimes=weather$StartDateTime[1:n]
endtimes=weather$StartDateTime[1:n]
for(i in 1:n){
  starttimes[i]=data[[i]]$StartDateTime[length(data[[i]]$StartDateTime)]
  endtimes[i]=data[[i]]$StartDateTime[1]
}
