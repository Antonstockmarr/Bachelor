#data.R
if(1==1){
  rm(list = ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
  
  #library("data.table")
  
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
    while(as.POSIXlt(x="2017-12-31 23:00:00",tz="GMT", format = "%Y-%m-%d %H:%M:%S")<=dt.tmp$StartDateTime[length(dt.tmp$StartDateTime)]){
      dt.tmp<-dt.tmp[1:(length(dt.tmp$StartDateTime)-1),]
    }
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
  weather<-weather[dim(weather)[1]:1,]
  
  rm(i,file.names,data.path,dt.tmp,Datalengths)
}
#exploratory.R
if(2==3){
  rm(list = ls())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
  
  source("data.R")
  
  pairs(data[[1]])
  
  plot(data[[1]]$Flow)
  plot.months <- c("January", "February", "March", "April", "May", "June", "July", "August", 
                   "September", "November", "December")
  
  par(mfrow = c(1,1))
  time <- seq(as.Date(StartDays[1]),as.Date(EndDays[1]), by = "1 mon")
  plot(data[[1]]$StartDateTime, data[[1]]$Flow, "l", xaxt = 'n')
  drawxaxis(data[[1]]$StartDateTime, tick.tstep="months") 
  axis.Date(1, at = seq(min(time), max(time), by = "12 mon"), format = "%m")
  
  # Investigating each house's flow behaviour 
  for (i in 1:1){
    #data[[i]]$StartDateTime.new <- as.POSIXlt(data[[i]]$StartDateTime)
    #data[[i]]$StartDateTime <- as.Date(data[[i]]$StartDateTime, "%Y-%m-%d")
    #data[[i]]$StartDateTime2 <- as.Date(cut(data[[i]]$StartDateTime, breaks = "month"))
    plot(data[[i]]$StartDateTime, data[[i]]$Flow, type = "l", xlab ="Time", ylab = "Flow")
    axis(1, at = unique(months(data[[i]]$StartDateTime)), las = 2)
  }
}

str(data[[1]])

tmp<-weather[weather$StartDateTime<=EndDays[1],]
tmp<-tmp[tmp$StartDateTime>=StartDays[1],]

str(tmp)
str(data[[1]])
str(weather)

colsc=c(1,rgb(132,202,41,maxColorValue = 255),rgb(231,176,59,maxColorValue = 255),rgb(229,56,50,maxColorValue = 255))

plot(data[[1]]$Flow,col=colsc[1])
