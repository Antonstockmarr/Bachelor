rm(list = ls())
library(xts)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
source("DataChecking.R")

# Loading all data
data.path = "../Watts_DistrictHeatingData_2018/"
file.names <- dir(data.path, pattern =".csv")
n <- length(file.names)
Datalengths = rep(c(1,n),nrow=n)
data <- vector(mode="list", length = n)
day.data <- vector(mode="list", length = n)


# Loading a single table to initialize dates
dt.tmp <- read.table(paste(data.path,file.names[1], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
names(dt.tmp)[1] = 'StartDateTime'
StartDays <- strptime(dt.tmp$StartDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
EndDays <- strptime(dt.tmp$StartDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
k <- 0;

for(i in 1:n){
  if (i == 5){
    dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
  }
  else
  {
    dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
    dt.tmp$X <- NULL
  }
  dt.tmp <- dt.tmp[,-1]
  dt.tmp$EndDateTime <- strptime(dt.tmp$EndDateTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  
  # Removing data before startdate of weather data
  #while(as.POSIXlt(x="2017-12-31 23:00:00",tz="GMT", format = "%Y-%m-%d %H:%M:%S")>=dt.tmp$StartDateTime[length(dt.tmp$StartDateTime)]){
  #  dt.tmp<-dt.tmp[1:(length(dt.tmp$StartDateTime)-1),]
  #}
  
  # Add logical vairable for weekends
  tmp.wd <- as.Date(dt.tmp$EndDateTime,tz="GMT")
  tmp.wd <-weekdays(tmp.wd,abbreviate = TRUE)
  dt.tmp$Weekend <- grepl("ø",tmp.wd)
  
  #Making daily data
  tmp.dat <- dt.tmp
  tmp.dat$EndDateTime <- as.Date(tmp.dat$EndDateTime,tz="GMT")
  tmp.dat$Obs <- rep(1,length(tmp.dat$EndDateTime))
  tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
  tmp.d2 <-aggregate(x=tmp.dat[,9],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
  tmp.dat <-data.frame(tmp.d1[,-9],Obs=tmp.d2[,2])
  day.data[[i]] <-tmp.dat
  
  # Fill missing null values.
  tmp.xts <- xts(dt.tmp[,-2:-1], order.by=dt.tmp[,1])
  t1<-rev(seq(from=tail(dt.tmp$EndDateTime,n=1), to=dt.tmp$EndDateTime[1], by="hour"))
  d1 <- xts(rep(1,length(t1)), order.by=t1)
  x <- merge(d1,tmp.xts,all=TRUE)
  tmp.df <- data.frame(EndDateTime=index(x),coredata(x))
  dt.tmp <- tmp.df[dim(tmp.df)[1]:1,]
  
  #Datalengths[i] = length(dt.tmp)
  
  
  # # Setting parameters for data checking
  # par = c('min_obs'=1000, 'miss_fraction'=1/20)
  # 
  # # If the data check is ok, store that data set
  # if (DataChecking(dt.tmp,par)==TRUE)
  # {
  #   k=k+1
  #   data[[k]] <- dt.tmp
  #   # Setting start and end times for each table.
  #   EndDays[k]= data[[k]]$EndDateTime[1]
  #   StartDays[k]=data[[k]]$EndDateTime[length(dt.tmp$EndDateTime)]
  # }
  
  
  
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

# Making temporary weather data in order to merge it with the house data
weather <- weather[dim(weather)[1]:1,]
tmp <- weather[(weather$StartDateTime <= EndDays[42]),]
tmp <- tmp[tmp$StartDateTime >= StartDays[42],]

#Making daily weather data
tmp.dat <- weather
tmp.dat$StartDateTime <- as.Date(tmp.dat$StartDateTime,tz="GMT")
tmp.dat$Obs <- rep(1,length(tmp.dat$StartDateTime))
tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
tmp.d2 <-aggregate(x=tmp.dat[,13],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
tmp.dat <-data.frame(tmp.d1[,-13],Obs=tmp.d2[,2])
day.weather <-tmp.dat


rm(i,file.names,data.path,dt.tmp,Datalengths,sStartDays,sEndDays,tmp,x,tmp.df,tmp.xts,t1,d1,weatherEnd,weatherStart,tmp.wd,tmp.dat)






















if("Til"=="Ida"){
tot.time.pts <-seq(from=min(StartDays), to=max(EndDays), by="hour")
avgconsumption<-rep(0,length(tot.time.pts))
weightavg<-rep(0,length(tot.time.pts))

for (i in 1:n) {
  tmp.index<-1+difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")
  tmp.data=data[[i]]
  tmp.data[is.na(data[[i]])] <- 0
  avgconsumption[tmp.index] <- avgconsumption[tmp.index] + tmp.data$Flow*tmp.data$CoolingDegree
  weightavg[tmp.index] <- weightavg[tmp.index] + rep(1,length(tmp.data$Flow)) - is.na(data[[i]]$Flow)
}

avgconsumption<-avgconsumption/weightavg
head(avgconsumption)

m=7

avgdata <- data[[2]]
avgdata[,1]<-seq(from=min(StartDays), to=max(EndDays), by="hour")
for(j in 2:m){
  avgdata[,j] <- rep(0,length(avgdata[[1]]))
  for (i in 1:n){
    tmp.index<-1+difftime(StartDays[i],min(StartDays), units ="hours"):difftime(EndDays[i],min(StartDays), units ="hours")
    tmp.data=data[[i]][,j]
    tmp.data[is.na(data[[i]])] <- 0
    avgdata[[j]][tmp.index] <- avgdata[[j]][tmp.index] + tmp.data
  }
  avgdata[[j]] <- avgdata[[j]]/weightavg
}
avgdata[,8]<-avgconsumption

ggplot(avgdata[,c(2,7)])

}
