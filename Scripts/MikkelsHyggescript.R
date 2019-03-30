setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("data.R")

# Loading all data
data.path = "../Consumption data/"
file.names <- dir(data.path, pattern =".csv")
n <- length(file.names)
Datalengths = rep(c(1,n),nrow=n)
data <- vector(mode="list", length = n)
day.data <- vector(mode="list", length = n)
data.key <- rep("",n)

# Loading a single table to initialize dates
dt.tmp <- read.table(paste(data.path,file.names[1], sep = ""), sep=";", stringsAsFactors=FALSE, header = TRUE, dec=',')
names(dt.tmp)[1] = 'StartDateTime'
StartDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
EndDays <- strptime(dt.tmp$EndDateTime[1:n], format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
k <- 0;


i=2

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
dt.tmp$Weekend <- grepl("?",tmp.wd)


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
  EndDays[k]= data[[k]]$ObsTime[1]
  StartDays[k]=data[[k]]$ObsTime[length(dt.tmp$ObsTime)]
  
  #Making daily data
  tmp.dat <- dt.tmp.noNA
  tmp.dat$ObsTime <- as.Date(tmp.dat$ObsTime,tz="GMT")
  tmp.dat$Obs <- rep(1,length(tmp.dat$ObsTime))
  tmp.d1 <-aggregate(x=tmp.dat[,-1],by= data.frame(Date = tmp.dat[,1]),FUN = mean)
  tmp.d2 <-aggregate(x=tmp.dat[,9],by= data.frame(Date = tmp.dat[,1]),FUN = sum)
  tmp.dat <-data.frame(tmp.d1[,-9],Obs=tmp.d2[,2])
  tmp.dat <- tmp.dat[dim(tmp.dat)[1]:1,]
  
  # Fill missing null values.
  tmp.xts <- xts(tmp.dat[,-1], order.by=tmp.dat[,1])
  t1<-rev(seq(from=tail(tmp.dat$Date,n=1), to=tmp.dat$Date[1], by="day"))
  d1 <- xts(rep(1,length(t1)), order.by=t1)
  x <- merge(d1,tmp.xts,all=TRUE)
  tmp.df <- data.frame(Date=index(x),coredata(x[,-1]))
  tmp.dat <- tmp.df[dim(tmp.df)[1]:1,]
  
  day.data[[k]] <-tmp.dat
  data.key[k]<-substr(file.names[i],1,36)
  
  
}


