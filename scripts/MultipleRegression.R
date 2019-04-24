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
for (i in 1:n)
{
  model.data[[i]]$Date <- NULL
  model.data[[i]]$PrecipitationProbability <- NULL
  model.data[[i]]$SunHour <- NULL
}
lmMultiple <- vector(mode="list", length = n)

 for (i in 1:n) {
   print(paste('Modeling house ',i))
   model.tmp <- model.data[[i]]
   model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
   Splinebasis <- BSplines(model.tmp$WindDirection)
   lmMultiple[[i]] <- stepP(lm(Consumption ~ Temperature*(I(WindSpeed*Splinebasis)[,1]+I(WindSpeed*Splinebasis)[,2]+I(WindSpeed*Splinebasis)[,3]+I(WindSpeed*Splinebasis)[,4])+(.-UltravioletIndex-WindSpeed)^2, data = model.tmp))
 }

# 
summary(lmMultiple[[60]]$object)
par(mfrow=c(2,2))

for (i in 1:n)
{
  plot(lmMultiple[[i]]$object)
}
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
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  MultiModel <- stepP(lm(Consumption ~ Temperature*WindSpeed*Splinebasis+(.-UltravioletIndex)^2, data = model.tmp))
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
