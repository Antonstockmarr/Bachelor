setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if(!exists("n")){
  source("data.R")
}
if(FALSE){
  Summer<-c(as.POSIXct(as.Date('2018-05-15'),format = "%Y-%m-%d", tz = "GMT"),as.POSIXct(as.Date('2018-09-15'),format = "%Y-%m-%d", tz = "GMT"))

  tmp<-data[[1]][data[[1]]$ObsTime>Summer[1],]
  tmp<-tmp[tmp$ObsTime<Summer[2],]
  
  tmpc<-tmp$Volume*tmp$CoolingDegree

  hour(tmp$ObsTime)

  plot(hour(tmp$ObsTime),tmpc)#,type='l')
}

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
  print(paste('Modelling house ',i))
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
  print(paste('Modelling house ',i))
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














