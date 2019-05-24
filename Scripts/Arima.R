library("forecast")

# The first model
two_sd <- data.frame("ar1"=0,"ma1"=0,'ar2'=0,'ma2'=0,'sar1'=0,'sma1'=0,'intercept'=0, "Temperature"=0)
three_sd <- data.frame("ar1"=0,"ma1"=0,'ar2'=0,'ma2'=0,'sar1'=0,'sma1'=0,'intercept'=0,"Temperature"=0)

logavg <-0

# 18 and 55 for report
for(i in 64:n){
A <- arima(weatherCons[[i]]$Consumption, order =c(2,2,2), seasonal = list(order = c(0,1,1), period = 24),xreg=weatherCons[[i]]$Temperature)
lagmax = 4*24
print(i)
par(mfrow=c(2,1))
acf(A$residuals,panel.first = c(abline(v=(1:4)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
title(paste("House ",i))
acf(A$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),main="")
logavg = logavg + A$loglik/n

names(two_sd)[length(two_sd)] <- names(A$coef)[length(names(A$coef))]
names(three_sd)[length(three_sd)] <- names(A$coef)[length(names(A$coef))]
for (j in names(A$coef))
{
  
  if (abs(A$coef[j])< 2*sqrt(diag(vcov(A)))[j])
  {
    two_sd[j]=two_sd[j]+1
  }
  if (abs(A$coef[j])< 3*sqrt(diag(vcov(A)))[j])
  {
    three_sd[j]=three_sd[j]+1
  }
}

}



