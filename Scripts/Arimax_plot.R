load("Arimax.Rdata")
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))


A18 <- arimax[[4]][[18]]
A55 <- arimax[[4]][[55]]
plot(A55)
plot(A18)


days = 2
lagmax = days*24+2
par(mfrow=c(2,1))
acf(A55$residuals,panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
title("House 55")
acf(A55$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")



days = 7
lagmax = days*24+2
par(mfrow=c(2,1))
acf(A55$residuals,panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
title("House 55")
acf(A55$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")

days = 2
lagmax = days*24+2
par(mfrow=c(2,1))
acf(A18$residuals,panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
title("House 18")
acf(A18$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")



days = 7
lagmax = days*24+2
par(mfrow=c(2,1))
acf(A18$residuals,panel.first = c(abline(v=(1:days)*24,col=Wcol[4],lty=2)),lag.max=lagmax,main="")
title("House 18")
acf(A18$residuals,"partial",lag.max = lagmax,panel.first = c(abline(v=(1:days)*24,col="red",lty=2)),main="")

