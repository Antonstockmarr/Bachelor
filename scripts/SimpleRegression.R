rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")


model.data <- day.data
for (i in 1:1) {
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),]
  
  model.tmp <- day.data[[i]]
  
  model.tmp <- model.tmp[day.tmp$Temperature <= 12,]
  model.data[[i]] <- model.tmp
  day.tmp <- day.tmp[day.tmp$Temperature <= 12,]
  
  lm.simple <- lm((model.data[[i]]$CoolingDegree*model.data[[i]]$Volume) ~ day.tmp$Temperature)
  # Checking model assumptions 
  par(mfrow = c(2,2))
  plot(lm.simple)
}
plot((model.data[[1]]$CoolingDegree*model.data[[1]]$Volume) ~ day.tmp$Temperature)
abline(lm.simple)

