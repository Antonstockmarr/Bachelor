library("forecast")

for(i in 1:n){
  nas<-which(!is.na(data[[i]]$Flow))
  tmp.dat<-data[[i]][nas[1]:tail(nas,1),]
  nas<-which(is.na(tmp.dat$Flow))
  print(nas)
  m<-dim(tmp.dat)[2]
  for(j in nas){
    tmp.dat[j,2:m]<-(data[[i]][j-1,2:m]+data[[i]][j+1,2:m])/2
  }
  data[[i]]<-tmp.dat
}

ARIMAX_model <- function(two_sd,three_sd,nonseas,seas)
{
  logavg <- 0
  for(i in 1:1){
    a <- 12
    tmp.dat <- weather[(weather$ObsTime <= head(data[[i]]$ObsTime,1)),]
    tmp.dat <- tmp.dat[tmp.dat$ObsTime >= tail(data[[i]]$ObsTime,1),]
    tmp <- tmp.dat$Temperature
    Temperature <- (tmp<a)*(a-tmp)
    arima.dat <- data.frame(Temperature = Temperature, Consumption = data[[i]]$CoolingDegree*data[[i]]$Volume)
    A <- arima(arima.dat$Consumption, order =nonseas, seasonal = list(order = seas, period = 24),xreg=arima.dat$Temperature)
    lagmax = 4*24
    print(i)
    par(mfrow=c(2,1))
    acf(A$residuals,panel.first = c(abline(v=(1:4)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
    title(paste("House ",i))
    acf(A$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),main="")
    logavg = logavg + A$loglik/n
    
    plot(A)
    
    names(two_sd)[length(two_sd)] <- names(A$coef)[length(names(A$coef))]
    names(three_sd)[length(three_sd)] <- names(A$coef)[length(names(A$coef))]
    for (j in names(A$coef))
    {
      
      if (abs(A$coef[j])< 2*sqrt(abs(diag(vcov(A))))[j])
      {
        two_sd[j]=two_sd[j]+1
      }
      if (abs(A$coef[j])< 3*sqrt(abs(diag(vcov(A))))[j])
      {
        three_sd[j]=three_sd[j]+1
      }
    }
    
  }
  result <- vector(mode='list',length=3)
  result[[1]] <- two_sd
  result[[2]] <- three_sd
  result[[3]] <- logavg
  result
}


# The first model
two_sd <- data.frame("ar1"=0,"ma1"=0,'ar2'=0,'ma2'=0,'sma1'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'ar2'=0,'ma2'=0,'sma1'=0,"Temperature"=0)

model1 <- ARIMAX_model(two_sd,three_sd,c(2,1,2),c(0,0,1))


# The next model
two_sd <- data.frame("ar1"=0,"ar2"=0,"ma1"=0,'sma1'=0,"Temperature"=0)
three_sd <- data.frame("ar1"=0,"ar2"=0,"ma1"=0,'sma1'=0,"Temperature"=0)

model2 <- ARIMAX_model(two_sd,three_sd,c(2,1,1),c(0,0,1))



# The next model
two_sd <- data.frame("ar1"=0,"ar2"=0,"ma1"=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ar2"=0,"ma1"=0, "Temperature"=0)

model3 <- ARIMAX_model(two_sd,three_sd,c(2,1,1),c(0,0,0))


# The next model
two_sd <- data.frame("ar1"=0,"ma1"=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0, "Temperature"=0)

model4 <- ARIMAX_model(two_sd,three_sd,c(1,1,1),c(0,0,0))


two_sd <- data.frame("ar1"=0,"ma1"=0,'sar1'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'sar1'=0,"Temperature"=0)

model5 <- ARIMAX_model(two_sd,three_sd,c(1,1,1),c(1,0,0))
