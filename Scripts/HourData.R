source("Data.r")
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
      Attribute[j+2] <- weather[(hour(data[[i]]$ObsTime) == j) && as.Date(data[[i]]$ObsTime)<=as.Date(EndDays[i]) && as.Date(data[[i]]$ObsTime)>=as.Date(StartDays[i]),k]
    }
    HourTmp[[k-1]] <- Attribute
  }
  
  HourData[[i]]<- data.frame(Consumption = Cons, Temperature = HourTmp[1], WindSpeed = HourTmp[2],
                             WindDirection = HourTmp[3], SunHour = HourTmp[4], Condition = HourTmp[5],
                             UltravioletIndex = HourTmp[6], MeanSeaLevelPressure = HourTmp[7], DewPoint = HourTmp[8],
                             Humidity = HourTmp[9], PrecipitationProbability = HourData[10],
                             IsHistoricalEstimated = HourTmp[11], Radiation = HourTmp[12])
}
