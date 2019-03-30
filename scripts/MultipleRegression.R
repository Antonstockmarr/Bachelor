rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
source("stepP.R")
library(foreach)
library(doParallel)

# setup parallel
cores = detectCores()

# Defining new data set where the summer period is left out
model.data <- weatherCons
lmMultiple <- vector(mode="list", length = n)

for (i in 1:2) {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]

  lmMultiple[[i]] <- stepP(lm(Consumption ~ Temperature*WindSpeed*WindDirection*SunHour*Condition*
                     UltravioletIndex*MeanSeaLevelPressure+Holiday, data = model.tmp))
}

summary(lmMultiple$object)
plot(lmMultiple$object)

plot(Consumption~Temperature,data=model.tmp)
lines(lmMultiple$object)
# 
# plot(lmMultiple[[1]])
par(mfrow = c(2,2))
plot(lm.multiple)

c <- makeCluster(cores[1]-1)
registerDoParallel(c)

lmMultiple <- foreach(i=1:n) %dopar% {
  day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[i],tz="GMT")),]
  day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[i],tz="GMT"),]
  
  model.tmp <- day.data[[i]]
  
  model.tmp <- model.tmp[day.tmp$Temperature <= 12,]
  model.data[[i]] <- model.tmp
  day.tmp <- day.tmp[day.tmp$Temperature <= 12,]
  
  lmMultiple[[i]] <- lm((model.data[[i]]$CoolingDegree*model.data[[i]]$Volume) ~ day.tmp$Temperature*day.tmp$WindSpeed*day.tmp$WindDirection*day.tmp$SunHour*day.tmp$Condition*day.tmp$UltravioletIndex)
  step(lmMultiple[[i]])
  drop1(lmMultiple[[i]], test  = "F")
}
#stop cluster
stopCluster(c)
