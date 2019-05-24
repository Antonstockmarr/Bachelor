library("forecast")
for(i in 1:n){
A <- arima(weatherCons[[i]]$Consumption, order =c(1,0,1), seasonal = list(order = c(0,0,1), period = 24),xreg=weatherCons[[i]]$Temperature)
lagmax = 4*24
print(i)
plot(A$residuals)
acf(A$residuals,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),lag.max=lagmax,main="")
Sys.sleep(1.5)
acf(A$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:4)*24,col="red",lty=2)),main="")
Sys.sleep(0.5)
}
par(mfrow=c(1,1))
qqnorm(A$residuals)
qqline(A$residuals)

plot(A)

A

