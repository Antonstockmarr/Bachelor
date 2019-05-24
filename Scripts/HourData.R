setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.r")
HourData <- vector("list",length=n)
rotate <- function(x) t(apply(x, 2, rev))

Avgcons <- c(1:n)
for (i in 1:n)
{
  Avgcons[i] <- mean(weatherCons[[i]]$Consumption,na.rm = TRUE)
}

for (i in 1:n)
{
  print(i)
  Cons <- data.frame(matrix(ncol=25,nrow = length(day.data[[i]]$Date)))#  
  names(Cons) <- c("Date",levels(as.factor(hour(data[[i]]$ObsTime))))
  Cons$Date <- day.data[[i]]$Date
  for (j in 0:23)
  {
    Cons[j+2] <- data[[i]]$Volume[hour(data[[i]]$ObsTime) == j]*data[[i]]$CoolingDegree[hour(data[[i]]$ObsTime) == j]
  }
  # Byt fuck
  HourTmp <- vector("list",length=length(weather))
  for (k in 2:length(weather))
  {
    Attribute <- data.frame(matrix(ncol=25,nrow = length(day.data[[i]]$Date)))  
    names(Attribute) <- c("Date",levels(as.factor(hour(data[[i]]$ObsTime))))
    Attribute$Date <- day.data[[i]]$Date
    for (j in 0:23)
    {
      tmp <- weather[weather$ObsTime <= data[[i]]$ObsTime[1],]
      tmp <- tmp[tmp$ObsTime >= tail(data[[i]]$ObsTime,1),]
      Attribute[j+2] <- tmp[(hour(data[[i]]$ObsTime) == j),k]
    }
    HourTmp[[k-1]] <- data.matrix(Attribute)
  }
  
  HourData[[i]]<- list(Consumption = data.matrix(Cons), Temperature = HourTmp[[1]], WindSpeed = HourTmp[[2]],
                            WindDirection = HourTmp[[3]], SunHour = HourTmp[[4]], Condition = HourTmp[[5]],
                            UltravioletIndex = HourTmp[[6]], MeanSeaLevelPressure = HourTmp[[7]], DewPoint = HourTmp[[8]],
                            Humidity = HourTmp[[9]], PrecipitationProbability = HourTmp[[10]],
                            IsHistoricalEstimated = HourTmp[[11]], Radiation = HourTmp[[12]])
}

Houravg = matrix(c(rep(0,24*n)),nrow = 24) 
for (i in 1:n)
  {
  SummerDays <- day.weather$Date[day.weather$Temperature >= 15]
  tmp <- HourData[[i]]$Consumption[,'Date']
  tmp_index <- sapply(tmp,function(x) x %in% SummerDays)
  Houravg[,i] <- sapply(HourData[[i]]$Consumption[tmp_index,],FUN=mean,na.rm = TRUE)
  }
rownames(Houravg) <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')
colnames(Houravg) <- c(1:n)
tt <- Houravg
library('fields')

image.plot(t(tt[rev(order(row.names(tt))),]), axes=FALSE, 
           lab.breaks=NULL,main = 'Average consumption of all houses during the day')
axis(2, at=seq(1+1/48,0-1/48, length=13), labels=c('00','02','04','06','08','10','12','14','16','18','20','22','24'), lwd=0.1, pos=-0.01,las=1)
abline(h=c(seq(1,0, length=24)+1/48),lwd=0.75)

tmp <-do.call(rbind,HourData[[1]]$Temperature)
HourData[[1]]$Temperature <- tmp
str(HourData[[1]])

HourData[[1]]$Temperature['Date',80]


tmp <- as.matrix(Attribute)
