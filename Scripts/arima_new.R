load("Data.RData")
library("forecast")
library("marima")

# House number
i=2
arima.dat <- cbind(Consumption,Temperature)

# arima
A <- arima(arima.dat[,1], order =c(1,0,1), seasonal = list(order = c(1,1,1), period = 24),xreg=arima.dat[,2])


# marima
differencing = c(1, 24)
dd <- define.dif(arima.dat, differencing)
dm1 <-define.model(kvar=2, ar = c(1,24,25), ma = c(1,24,25),reg.var = 2)
dm1$ar.pattern[1,2,25:26] <- 0
m1 <- marima(dd$y.dif, ar.pattern = dm1$ar.pattern, ma.pattern = dm1$ma.pattern, Plot = 'log.det',penalty=0)


#comparing
short.form(m1$ar.estimates,leading = F)
A
short.form(m1$ma.estimates,leading = F)

## for i=2
# arima:
# ar(1) = 0.97
# ma(1) = -0.82
# seasonal ar(1) = 0.014
# seasonal ma(1) = -0.95
# reg.var = 0.05

# marima:
# ar(1) = -0.61
# ma(1) = -0.41
# seasonal ar(1) = -0.002
# seasonal ma(1) = -0.81
# reg.var = -0.001



A <- arima(Consumption, order =c(1,0,1))
#differencing = c(1, 24)
#dd <- define.dif(arima.dat[,1], differencing)
dm1 <-define.model(kvar=1, ar = c(1), ma = c(1))
m1 <- marima(Consumption, ar.pattern = dm1$ar.pattern, ma.pattern = dm1$ma.pattern, Plot = 'log.det')
short.form(m1$ar.estimates)
short.form(m1$ma.estimates)
