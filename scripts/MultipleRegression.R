rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
source("stepP.R")
source("BSplines.R")
library(foreach)
library(doParallel)

# setup parallel
cores = detectCores()

# Defining new data set where the summer period is left out
model.data <- weatherCons
lmMultiple <- vector(mode="list", length = n)
# 
 for (i in 10) {
   model.tmp <- model.data[[i]]
   model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
# 
   lmMultiple[[i]] <- stepP(lm(Consumption ~ (Temperature+WindSpeed+WindDirection+SunHour+Condition+
                      UltravioletIndex+MeanSeaLevelPressure)^3+Holiday, data = model.tmp))
   
 }
# 
summary(lmMultiple[[5]]$object)
plot(lmMultiple[[2]]$object)
# 
# plot(Consumption~Temperature,data=model.tmp)
# lines(lmMultiple$object)
# # 
# # plot(lmMultiple[[1]])
# par(mfrow = c(2,2))
# plot(lm.multiple)
# 
# plot(lmMultiple[[1]])

c <- makeCluster(cores[1]-1)
registerDoParallel(c)


lmMultiple <- foreach(i=1:n) %dopar% {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  MultiModel <- stepP(lm(Consumption ~ (Temperature+WindSpeed+WindDirection+SunHour+Condition+
                                UltravioletIndex+MeanSeaLevelPressure+Holiday)^2 +
                          (WindSpeed*Splinebasis)^2, data = model.tmp))
  MultiModel
}
#stop cluster
stopCluster(c)

modelListSlope <- vector(mode="list",length=n)
modelListPval <- vector(mode="list",length=n)

for (i in 1:n)
{
  modelListSlope[[i]] = lmMultiple[[i]]$object #for slopes
  modelListPval[[i]] = summary(lmMultiple[[i]]$object)$coefficients #for p vals
}
