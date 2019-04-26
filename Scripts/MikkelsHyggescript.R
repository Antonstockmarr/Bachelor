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
  model.data[[i]]$UltravioletIndex <- NULL
}
lmMultiple <- vector(mode="list", length = n)

lmMultipleNoP <- vector(mode="list", length = n)

CircleColoring <- function(splines,modelobject){
  colors=rep(0,length(splines[,1]))
  f<-summary(modelobject)$coefficients
  
  N<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 1]"))
  E<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 2]"))
  S<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 3]"))
  W<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 4]"))
  
  ncol=Wcol[2]
  if(sum(N)==1){
    p<-f[match(TRUE,N),4]
    if(p<0.05){
      ncol=Wcol[3]
    }
    if(p<0.01){
      ncol=Wcol[4]
    }
  }
  ecol=Wcol[2]
  if(sum(E)==1){
    p<-f[match(TRUE,E),4]
    if(p<0.05){
      ecol=Wcol[3]
    }
    if(p<0.01){
      ecol=Wcol[4]
    }
  }
  scol=Wcol[2]
  if(sum(S)==1){
    p<-f[match(TRUE,S),4]
    if(p<0.05){
      scol=Wcol[3]
    }
    if(p<0.01){
      scol=Wcol[4]
    }
  }
  wcol=Wcol[2]
  if(sum(W)==1){
    p<-f[match(TRUE,W),4]
    if(p<0.05){
      wcol=Wcol[3]
    }
    if(p<0.01){
      wcol=Wcol[4]
    }
  }
  
  colors[splines[,3]==0]<-ncol
  colors[splines[,4]==0]<-ecol
  colors[splines[,1]==0]<-wcol
  colors[splines[,2]==0]<-scol
  return(colors)
}



for (i in 1:n) {
  
  print(paste('Modelling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  if(length(weatherCons[[i]]$Date<360)){
    lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(I(WindSpeed*Splinebasis)[,1]+I(WindSpeed*Splinebasis)[,2]+I(WindSpeed*Splinebasis)[,3]+I(WindSpeed*Splinebasis)[,4])+AutumnBreak+ChristmasBreak+Weekend+(.-WindSpeed-Weekend-AutumnBreak-SpringBreak-ChristmasBreak-WinterBreak)^2, data = model.tmp)
  }else{
    lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(I(WindSpeed*Splinebasis)[,1]+I(WindSpeed*Splinebasis)[,2]+I(WindSpeed*Splinebasis)[,3]+I(WindSpeed*Splinebasis)[,4])+AutumnBreak+ChristmasBreak+SpringBreak+WinterBreak+Weekend+(.-WindSpeed-Weekend-AutumnBreak-SpringBreak-ChristmasBreak-WinterBreak)^2, data = model.tmp)
  }
  lmMultiple[[i]] <- stepP(lmMultipleNoP[[i]])
  
  BSplin <- matrix(data=Splinebasis %*% diag(4),ncol=4)
  Knot <- matrix(c(0,1,1,0,0,-1,-1,0),nrow=4,byrow=T)
  Spline <- (BSplin)%*%Knot
  plot(Spline[,1],Spline[,2],xlim=c(-1,1),ylim=c(-1,1),col=CircleColoring(Splinebasis,lmMultiple[[i]]$object),main = paste('Dependency on the wind direction for house ',i),xlab='West - East',ylab='South - North')
  abline(h=0,v=0)
  
}

summary(lmMultipleNoP[[1]])


# 
summary(lmMultiple[[49]]$object)
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














