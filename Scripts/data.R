rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

# Sources and packages
source("DataChecking.R")
source("Polarize.R")
source("Sun.R")
library(xts)

# Watts colors
Wcol=c(1,rgb(132,202,41,maxColorValue = 255),rgb(231,176,59,maxColorValue = 255),rgb(229,56,50,maxColorValue = 255))


# Loading all data
data.path = "../Consumption data/"
file.names <- dir(data.path, pattern =".csv")
n <- length(file.names)
Datalengths = rep(1,nrow=n)
data <- vector(mode="list", length = n)
day.data <- vector(mode="list", length = n)
data.key <- rep("",n)

Weekend=weekdays(as.POSIXlt(c(as.Date('2019-01-26'),as.Date('2019-01-27')),format = "%Y-%m-%d", tz = "GMT"),abbreviate = TRUE)
sat<-substring(Weekend[1],1:2,1:2)
sun<-substring(Weekend[2],1:2,1:2)


# Loading a single table to initialize dates
dt.tmp <- read.table(paste(data.path,file.names[1], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
names(dt.tmp)[1] = 'StartDateTime'
StartDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
EndDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
k <- 0;

for(i in 1:n){
  dt.tmp <- read.table(paste(data.path,file.names[i], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
  dt.tmp$X <- NULL
  
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
  dt.tmp$Weekend <- grepl(intersect(sat,sun),tmp.wd)
  
  dt.tmp.noNA<- dt.tmp
  
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
    
    #Making daily data
    tmp.dat <- dt.tmp.noNA
    tmp.dat$ObsTime <- as.Date(tmp.dat$ObsTime,tz="GMT")
    tmp.dat$Obs <- rep(1,length(tmp.dat$ObsTime))
    tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
    tmp.d2 <-aggregate(x=tmp.dat[,9],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
    tmp.dat <-data.frame(tmp.d1[,-9],Obs=tmp.d2[,2])
    
    
    # Fill missing null values.
    tmp.xts <- xts(tmp.dat[,-1], order.by=tmp.dat[,1])
    t1<-rev(seq(from=tmp.dat$Date[1], to=tail(tmp.dat$Date,n=1), by="day"))
    d1 <- xts(rep(1,length(t1)), order.by=t1)
    x <- merge(d1,tmp.xts,all=TRUE)
    tmp.df <- data.frame(Date=index(x),coredata(x[,-1]))
    tmp.dat <- tmp.df[dim(tmp.df)[1]:1,]
    
    day.data[[k]] <-tmp.dat
    data.key[k]<-substr(file.names[i],1,36)
  }
  
  
  
}

if (k<n){
  data<-data[-(k+1:n)]
  day.data<-day.data[-(k+1:n)]
  data.key<-data.key[-(k+1:n)]
  Datalengths<-Datalengths[-(k+1:n)]
}

# k is new n
n <- k

#Removing Feb data to get rid of NA

jan1<-day.data[[1]]$Date[1]

for(i in 1:n){
  while(day.data[[i]]$Date[1]>jan1){
    day.data[[i]]<-day.data[[i]][-1,]
    #weatherCons[[i]]<-weatherCons[[i]][-1,]
  }
  Datalengths[i]=length(day.data[[i]]$Date)
  EndDays[i]= day.data[[i]]$Date[1]
  StartDays[i]=day.data[[i]]$Date[length(day.data[[i]]$Date)]
  
}


tmp.df<-data.frame(Key=data.key)

BBR.tmp <- read.table('../BBRdata.csv', sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
BBR <- merge(tmp.df,BBR.tmp)
names(BBR)[5] <-  "Kaelder"
names(BBR)[7] <-  "Byggeaar"
names(BBR)[9] <-  "Ombygningsaar"



# Reading weather data  
weather <- read.table('../WeatherData_01-01-2018_02-06-2019.csv', sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
names(weather)[1]="ObsTime"
weather$ObsTime = strptime(weather$ObsTime,format='%d-%m-%Y %H:%M:%S',tz = 'GMT')
weather$IsHistoricalEstimated=weather$IsHistoricalEstimated=="True"
weather$X <- NULL
weather$Radiation <- Sun(weather$ObsTime[1],tail(weather$ObsTime,n=1))

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
# Making tmp.d1 the mean of every attribute for each day except for obs and sunHour.
tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
# Making tmp.d2 the sum of obs and sunhour
tmp.d2 <-aggregate(x=tmp.dat[,c(5,14)],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
# Combining the two dataframes to a single one
tmp.dat <-data.frame(tmp.d1[,-c(5,14)],SunHour = tmp.d2[,2],Obs=tmp.d2[,3])
# Combining Radiation with sunhour
tmp.dat$Radiation <- tmp.dat$SunHour*tmp.dat$Radiation
day.weather <-tmp.dat
# Switching the rows, such that the newest days are first
day.weather <- day.weather[dim(day.weather)[1]:1,]


# WindSpeed and WindDirection
tmp.rekt <- matrix(data=rep(0,length(weather$ObsTime)*2),ncol=2)
tmp.rekt[,1] = sin(weather$WindDirection)*weather$WindSpeed
tmp.rekt[,2] = cos(weather$WindDirection)*weather$WindSpeed
tmp.coord <- aggregate(x=tmp.rekt,by=data.frame(Date = as.Date(weather$ObsTime,tz="GMT")),FUN = mean)
#tmp.polar <- apply(tmp.coord,MARGIN =c(2:3), FUN = Polarize,x="V1",y="V2")

tmp.polar <- matrix(rep(0,length(tmp.coord[,1])*2),ncol=2)
for (i in 1:length(tmp.coord[,1]))
{
tmp.polar[i,] <- Polarize(tmp.coord[i,2],tmp.coord[i,3])  
}
day.weather$WindSpeed <- tmp.polar[,1]
day.weather$WindDirection <- tmp.polar[,2]

# Making temporary weather data in order to merge it with the house data
day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[42],tz="GMT")),]
day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[42],tz="GMT"),]



# Making average daily data:

day.avg <- day.data[[match(max(Datalengths),Datalengths)]]
#day.avg[,1]<-seq(from=as.Date(min(StartDays),tz="GMT"), to=as.Date(max(EndDays),tz="GMT"), by="day")

m=dim(day.avg)[2]

for(j in 2:m){
  day.avg[,j] <- rep(0,length(day.avg[,1]))
  weightavg<-rep(0,length(day.avg[,1]))
  for (i in 1:n){
    tmp.index<-1+difftime(as.Date(max(EndDays),tz="GMT"),as.Date(EndDays[i],tz="GMT"), units ="day"):difftime(as.Date(max(EndDays),tz="GMT"),as.Date(StartDays[i],tz="GMT"), units ="day")
    tmp.data=day.data[[i]][,j]
    tmp.index<- tmp.index[!is.na(tmp.data)]
    day.avg[tmp.index,j] <- day.avg[tmp.index,j] + tmp.data
    weightavg[tmp.index] <- weightavg[tmp.index] + rep(1,length(tmp.data)) - is.na(day.data[[i]]$Flow)
  }
  day.avg[,j] <- day.avg[,j]/weightavg
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
  day.tmp$Humidity<-NULL
  tmpcons <- day.data[[i]]$Volume*day.data[[i]]$CoolingDegree
  weatherCons[[i]]<-cbind(day.tmp,tmpcons)
  names(weatherCons[[i]])[names(weatherCons[[i]])=='tmpcons']<-"Consumption"
  weatherCons[[i]]$Obs <- NULL
}


# Adding vacation periods as attributes in day.data. Dates are taken from
# http://skoleferie-dk.dk/skoleferie-aalborg/?fbclid=IwAR1l2J2t9mHz8JC3qho9stqOj7k7e8MrJQ461a7Iy6_Ekf5AaL8HNzZY9WM
WinterBreakDates <- as.POSIXlt(seq(as.Date('2018-02-10'),as.Date('2018-02-18'), by="days"),format = "%Y-%m-%d", tz = "GMT")
SpringBreakDates <- as.POSIXlt(seq(as.Date('2018-03-24'),as.Date('2018-04-02'), by="days"),format = "%Y-%m-%d", tz = "GMT")
AutumnBreakDates <- as.POSIXlt(seq(as.Date('2018-10-13'),as.Date('2018-10-21'), by="days"),format = "%Y-%m-%d", tz = "GMT")
ChristmasBreakDates <- as.POSIXlt(seq(as.Date('2018-12-22'),as.Date('2019-01-02'), by="days"),format = "%Y-%m-%d", tz = "GMT")

for (i in 1:n)
{
  tmp_WinterBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% WinterBreakDates)[1,])
  tmp_SpringBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% SpringBreakDates)[1,])
  tmp_AutumnBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% AutumnBreakDates)[1,])
  tmp_ChristmasBreak <-as.integer(apply(day.data[[i]],1,function(x) x %in% ChristmasBreakDates)[1,])
  day.data[[i]]$Holiday <- as.factor(1*tmp_WinterBreak+2*tmp_SpringBreak+3*tmp_AutumnBreak+4*tmp_ChristmasBreak)
  levels(day.data[[i]]$Holiday) <- c('Working days', 'Winter break', 'Spring break', 'Autumn break', 'Christmas break')
  
  tmp_WinterBreak <-as.integer(apply(weatherCons[[i]],1,function(x) x %in% WinterBreakDates)[1,])
  tmp_SpringBreak <-as.integer(apply(weatherCons[[i]],1,function(x) x %in% SpringBreakDates)[1,])
  tmp_AutumnBreak <-as.integer(apply(weatherCons[[i]],1,function(x) x %in% AutumnBreakDates)[1,])
  tmp_ChristmasBreak <-as.integer(apply(weatherCons[[i]],1,function(x) x %in% ChristmasBreakDates)[1,])
  weatherCons[[i]]$Holiday <- as.factor(1*tmp_WinterBreak+2*tmp_SpringBreak+3*tmp_AutumnBreak+4*tmp_ChristmasBreak)
  levels(weatherCons[[i]]$Holiday) <- c('Working days', 'Winter break', 'Spring break', 'Autumn break', 'Christmas break')
}

tmp_WinterBreak <-as.integer(apply(day.avg,1,function(x) x %in% WinterBreakDates)[1,])
tmp_SpringBreak <-as.integer(apply(day.avg,1,function(x) x %in% SpringBreakDates)[1,])
tmp_AutumnBreak <-as.integer(apply(day.avg,1,function(x) x %in% AutumnBreakDates)[1,])
tmp_ChristmasBreak <-as.integer(apply(day.avg,1,function(x) x %in% ChristmasBreakDates)[1,])
day.avg$Holiday <- as.factor(1*tmp_WinterBreak+2*tmp_SpringBreak+3*tmp_AutumnBreak+4*tmp_ChristmasBreak)
levels(day.avg$Holiday) <- c('Working days', 'Winter break', 'Spring break', 'Autumn break', 'Christmas break')




rm(i,file.names,data.path,dt.tmp,Datalengths,sStartDays,sEndDays,tmp,x,tmp.df,tmp.xts,t1,d1,weatherEnd,weatherStart,tmp.wd,tmp.dat,tmp.d1,tmp.d2,par,day.tmp,tmp.data,tmp.index,weightavg,m,j,k,dt.tmp.noNA,BBR.tmp)