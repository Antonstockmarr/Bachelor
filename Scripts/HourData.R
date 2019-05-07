source("data.r")
HourData <- vector("list",length=n)



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
    HourTmp[[k-1]] <- Attribute
  }
  
  HourData[[i]]<- data.frame(Consumption = Cons, Temperature = HourTmp[[1]], WindSpeed = HourTmp[[2]],
                             WindDirection = HourTmp[[3]], SunHour = HourTmp[[4]], Condition = HourTmp[[5]],
                             UltravioletIndex = HourTmp[[6]], MeanSeaLevelPressure = HourTmp[[7]], DewPoint = HourTmp[[8]],
                             Humidity = HourTmp[[9]], PrecipitationProbability = HourTmp[[10]],
                             IsHistoricalEstimated = HourTmp[[11]], Radiation = HourTmp[[12]])
}
