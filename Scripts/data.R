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
StartDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
EndDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
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
  names(dt.tmp)[1]="ObsTime"
  dt.tmp$ObsTime <- strptime(dt.tmp$ObsTime, format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
  
  # Removing data before startdate of weather data
  #while(as.POSIXlt(x="2017-12-31 23:00:00",tz="GMT", format = "%Y-%m-%d %H:%M:%S")>=dt.tmp$ObsTime[length(dt.tmp$ObsTime)]){
  #  dt.tmp<-dt.tmp[1:(length(dt.tmp$ObsTime)-1),]
  #}
  
  # Add logical vairable for weekends
  tmp.wd <- as.Date(dt.tmp$ObsTime,tz="GMT")
  tmp.wd <-weekdays(tmp.wd,abbreviate = TRUE)
  dt.tmp$Weekend <- grepl("?",tmp.wd)
  
  #Making daily data
  tmp.dat <- dt.tmp
  tmp.dat$ObsTime <- as.Date(tmp.dat$ObsTime,tz="GMT")
  tmp.dat$Obs <- rep(1,length(tmp.dat$ObsTime))
  tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
  tmp.d2 <-aggregate(x=tmp.dat[,9],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
  tmp.dat <-data.frame(tmp.d1[,-9],Obs=tmp.d2[,2])
  day.data[[i]] <-tmp.dat[dim(tmp.dat)[1]:1,]
  
  # Fill missing null values.
  tmp.xts <- xts(dt.tmp[,-1], order.by=dt.tmp[,1])
  t1<-rev(seq(from=tail(dt.tmp$ObsTime,n=1), to=dt.tmp$ObsTime[1], by="hour"))
  d1 <- xts(rep(1,length(t1)), order.by=t1)
  x <- merge(d1,tmp.xts,all=TRUE)
  tmp.df <- data.frame(ObsTime=index(x),coredata(x[,-1]))
  dt.tmp <- tmp.df[dim(tmp.df)[1]:1,]
  
  #Datalengths[i] = length(dt.tmp)
  
  # Setting parameters for data checking
  par = c('min_obs'=1000, 'miss_fraction'=1/20)
  
  # If the data check is ok, store that data set
  if (DataChecking(dt.tmp,par)==TRUE)
  {
    k=k+1
    data[[k]] <- dt.tmp
    # Setting start and end times for each table.
    EndDays[k]= data[[k]]$ObsTime[1]
    StartDays[k]=data[[k]]$ObsTime[length(dt.tmp$ObsTime)]
  }
  
  
  
}

# Adding vacation periods as attributes in day.data. Dates are taken from
# http://skoleferie-dk.dk/skoleferie-aalborg/?fbclid=IwAR1l2J2t9mHz8JC3qho9stqOj7k7e8MrJQ461a7Iy6_Ekf5AaL8HNzZY9WM
WinterBreakDates <- as.POSIXlt(seq(as.Date('2018-02-10'),as.Date('2018-02-18'), by="days"),format = "%Y-%m-%d", tz = "GMT")
SpringBreakDates <- as.POSIXlt(seq(as.Date('2018-03-24'),as.Date('2018-04-02'), by="days"),format = "%Y-%m-%d", tz = "GMT")
AutumnBreakDates <- as.POSIXlt(seq(as.Date('2018-10-13'),as.Date('2018-10-21'), by="days"),format = "%Y-%m-%d", tz = "GMT")
ChristmasBreakDates <- as.POSIXlt(seq(as.Date('2018-12-22'),as.Date('2019-01-02'), by="days"),format = "%Y-%m-%d", tz = "GMT")

for (i in 1:n)
{
    day.data[[i]]$WinterBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% WinterBreakDates)[1,])
    day.data[[i]]$SpringBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% SpringBreakDates)[1,])
    day.data[[i]]$AutumnBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% AutumnBreakDates)[1,])
    day.data[[i]]$ChristmasBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% ChristmasBreakDates)[1,])
    day.data[[i]]$Holiday <- as.factor(1*day.data[[i]]$WinterBreak+2*day.data[[i]]$SpringBreak+3*day.data[[i]]$AutumnBreak+4*day.data[[i]]$ChristmasBreak)
    levels(day.data[[i]]$Holiday) <- c('Working days', 'Winter break', 'Spring break', 'Autumn break', 'Christmas break')
}

# Reading weather data  
weather <- read.table('../WeatherData_01-01-2018_09-05-2019.csv', sep="\t", stringsAsFactors=FALSE, header = TRUE, dec=',')
names(weather)[1]="ObsTime"
weather$ObsTime = strptime(weather$ObsTime,format='%d/%m/%Y %H.%M',tz = 'GMT')
weather$IsHistoricalEstimated=weather$IsHistoricalEstimated=="True"



# Sorting dates
sStartDays <- StartDays[order(StartDays)]
sEndDays <- EndDays[order(EndDays)]

weatherStart = weather$ObsTime[1]
weatherEnd = weather$ObsTime[length(weather$ObsTime[weather$IsHistoricalEstimated==FALSE])]
weather <- weather[dim(weather)[1]:1,]

# Making temporary weather data in order to merge it with the house data
tmp <- weather[(weather$ObsTime <= EndDays[42]),]
tmp <- tmp[tmp$ObsTime >= StartDays[42],]

#Making daily weather data
tmp.dat <- weather
tmp.dat$ObsTime <- as.Date(tmp.dat$ObsTime,tz="GMT")
tmp.dat$Obs <- rep(1,length(tmp.dat$ObsTime))
tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
tmp.d2 <-aggregate(x=tmp.dat[,13],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
tmp.dat <-data.frame(tmp.d1[,-13],Obs=tmp.d2[,2])
day.weather <-tmp.dat
day.weather <- day.weather[dim(day.weather)[1]:1,]


# Making temporary weather data in order to merge it with the house data
day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[42],tz="GMT")),]
day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[42],tz="GMT"),]


# Making average daily data:

day.avg <- day.data[[2]]
#day.avg[,1]<-seq(from=as.Date(min(StartDays),tz="GMT"), to=as.Date(max(EndDays),tz="GMT"), by="day")

m=dim(day.avg)[2]

for(j in 2:m){
  day.avg[,j] <- rep(0,length(day.avg[,1]))
  weightavg<-rep(0,length(day.avg[,1]))
  for (i in 1:n){
    tmp.index<-1+difftime(as.Date(StartDays[i],tz="GMT"),as.Date(min(StartDays),tz="GMT"), units ="day"):difftime(as.Date(EndDays[i],tz="GMT"),as.Date(min(StartDays),tz="GMT"), units ="day")
    tmp.data=day.data[[i]][,j]
    day.avg[tmp.index,j] <- day.avg[tmp.index,j] + tmp.data
    weightavg[tmp.index] <- weightavg[tmp.index] + rep(1,length(tmp.data)) - is.na(day.data[[i]]$Flow)
  }
  day.avg[[j]] <- day.avg[[j]]/weightavg
}

# Adding consumption attribute to daily avg. house data
day.avg$Consumption <- day.avg$Volume*day.avg$CoolingDegree


weatherCons <- vector(mode="list", length = n)
for (i in 1:n)
{
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),]
  day.tmp$IsHistoricalEstimated<-NULL
  day.tmp$DewPoint<-NULL
  tmpcons <- day.data[[i]]$Volume*day.data[[i]]$CoolingDegree
  weatherCons[[i]]<-cbind(tmpcons,day.tmp)
  names(weatherCons[[i]])[1]<-"Consumption"
}




rm(i,file.names,data.path,dt.tmp,Datalengths,sStartDays,sEndDays,tmp,x,tmp.df,tmp.xts,t1,d1,weatherEnd,weatherStart,tmp.wd,tmp.dat,tmp.d1,tmp.d2,par,day.tmp,tmp.data,tmp.index,weightavg,m,j)
