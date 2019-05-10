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
source("CircleCol.R")

# Defining new data set where the summer period is left out
model.data <- weatherCons
# Various attributes are removed 
for (i in 1:n)
{
  model.data[[i]]$Date <- NULL
  model.data[[i]]$PrecipitationProbability <- NULL
  model.data[[i]]$SunHour <- NULL
  model.data[[i]]$UltravioletIndex <- NULL
}


# Full regression model ---------------------------------------------------
lmMultiple <- vector(mode="list", length = n)
lmMultipleNoP <- vector(mode = "list", length = n)
lmSummary_est <- matrix(rep(0,11*n),nrow = n)
lmSummary_p <- matrix(rep(0,11*n),nrow = n)
colnames(lmSummary_est) <- c("I","T","W1","W2","W3","W4","SolaR","T:W1","T:W2","T:W3","T:W4")
colnames(lmSummary_p) <- c("I","T","W1","W2","W3","W4","SolaR","T:W1","T:W2","T:W3","T:W4")


par(mfrow = c(1,1))
for (i in 1:n) {
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  
  #  wd <- model.tmp$WindDirection[order(model.tmp$WindDirection)]
  #  wd[wd<45] <- wd[wd<45]+360
  tmp.wind <- Splinebasis*model.tmp$WindSpeed#[order(wd)]
  #  tmp.wind <- model.tmp$WindSpeed[order(model.tmp$WindDirection)]
  model.tmp$North <- tmp.wind[,3]
  model.tmp$East <- tmp.wind[,4]
  model.tmp$South <- tmp.wind[,1]
  model.tmp$West <- tmp.wind[,2]
  lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(North + East + South + West)+
                             Radiation, data = model.tmp)
  
  
  
  lmMultiple[[i]] <- stepP(lmMultipleNoP[[i]])
  
  
  
  lmSummary_est[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,1] 
  lmSummary_p[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,4] 
  #wd2 <- model.tmp$WindDirection[order(model.tmp$WindDirection)]
  plot(model.tmp$WindDirection,Splinebasis[,1]*lmSummary_est[i,'W1']+Splinebasis[,2]*lmSummary_est[i,'W2']
       +Splinebasis[,3]*lmSummary_est[i,'W3']+Splinebasis[,4]*lmSummary_est[i,'W4'],ylab='Dependency',xlab='Wind Direction',col=Wcol[2],
       main= i)
  abline(h=0)
  
  BSplin <- matrix(data=Splinebasis %*% diag(lmSummary_est[i,c('W1','W2','W3','W4')]),ncol=4)
  Knot <- matrix(c(0,1,1,0,0,-1,-1,0),nrow=4,byrow=T)
  Spline <- (BSplin)%*%Knot
  plot(Spline[,1],Spline[,2],col=CircleCol(Splinebasis,lmMultiple[[i]]$object),main = paste('Dependency on the wind direction for house ',i),xlab='West - East',ylab='South - North')
  abline(h=0,v=0)
  
}

f<-predict(lmMultipleNoP[[1]], se.fit = TRUE,interval = "confidence")

dim(f$fit)

?predict

model.tmp <- model.data[[1]]
model.tmp <- model.tmp[model.tmp$Temperature <= 12,]

length(model.tmp$Consumption)
length(f$fit)
plot(f$fit[,1],type='l')
lines(model.tmp$WindDirection,f$fit[,2])

summary(f)
