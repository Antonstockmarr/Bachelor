rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
# Defining new data set where the summer period is left out
model.data <- day.data
lmMultiple <- vector(mode="list", length = n)



for (i in 1:1) {
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),]
  
  model.tmp <- day.data[[i]]
  
  model.tmp <- model.tmp[day.tmp$Temperature <= 12,]
  model.data[[i]] <- model.tmp
  day.tmp <- day.tmp[day.tmp$Temperature <= 12,]
  
  lmMultiple[[i]] <- lm(model.data[[i]]$CoolingDegree*model.data[[i]]$Volume ~ day.tmp$Temperature*day.tmp$WindSpeed*day.tmp$WindDirection*day.tmp$SunHour*day.tmp$Condition*day.tmp$UltravioletIndex*day.tmp$MeanSeaLevelPressure*day.tmp$DewPoint*day.tmp$Humidity*day.tmp$PrecipitationProbability*model.data[[i]]$Holiday)
  par(mfrow = c(2,2))
#  plot(lm.multiple)
}
anova(lmMultiple[[1]])
step(lmMultiple[[1]])
plot(lmMultiple[[1]])
