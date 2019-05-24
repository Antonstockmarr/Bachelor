library("forecast")
A <- arima(weatherCons[[40]]$Consumption, order =c(1,1,1), seasonal = list(order = c(0,0,1), period = 24))
lagmax = 4*24

plot(A$residuals)
acf(A$residuals,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),lag.max=lagmax,main="")
acf(A$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),main="")
par(mfrow=c(1,1))
qqnorm(A$residuals)
qqline(A$residuals)

plot(A)

A

