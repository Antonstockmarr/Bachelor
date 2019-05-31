setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if(!exists("n")){
  source("data.R")
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
Wind.Pred <- vector(mode = "list", length = n)
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
  
  model.Wind<-data.frame(Consumption=model.tmp$Consumption,Temperature=model.tmp$Temperature,Radiation=model.tmp$Radiation,N=model.tmp$North,E=model.tmp$East,S=model.tmp$South,W=model.tmp$West)
  lmMultipleNoP[[i]] <- lm(Consumption ~ .+Temperature*(N + E + S + W),data = model.Wind)
  Splinebasis2 <- BSplines(1:360)
  newData = data.frame(Temperature = rep(0, 360), # 0 grader
                       Radiation = rep(0, 360), # Om natten
                       N = Splinebasis2[,3],
                       E = Splinebasis2[,4],
                       S = Splinebasis2[,1],
                       W = Splinebasis2[,2])
  
  Wind.Pred[[i]]<-data.frame(predict(object=lmMultipleNoP[[i]], newdata=newData, interval = "confidence", level = 0.95))
  
  plot(Wind.Pred[[i]]$fit,type='l',ylim=range(Wind.Pred[[i]]$lwr,Wind.Pred[[i]]$upr),main=paste("hus: ",i))
  lines(Wind.Pred[[i]]$upr,lty=2)
  lines(Wind.Pred[[i]]$lwr,lty=2)
  abline(v=c(0,90,180,270,360), col="gray", lty=2, lwd=1)
  
  lmMultiple[[i]] <- stepP(lmMultipleNoP[[i]])
  
  lmSummary_est[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,1] 
  lmSummary_p[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,4] 
}
summary(lmMultipleNoP[[i]])

        