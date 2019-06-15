# Physical MARIMA
i<- 20
Consumption <- data[[i]]$Volume*data[[i]]$CoolingDegree*cc

a <- 12
tmp.dat <- weather[(weather$ObsTime >= head(data[[i]]$ObsTime,1)),]
tmp.dat <- tmp.dat[tmp.dat$ObsTime <= tail(data[[i]]$ObsTime,1),]
tmp <- tmp.dat$Temperature
Temperature <- (tmp<a)*(a-tmp)
Radiation <- tmp.dat$Radiation

len <- length(data[[i]]$ObsTime)
sin1 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/24)
sin2 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/12)
cos1 <- cos(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/24)
cos2 <- cos(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/12)
cos3 <- cos((((1:len)-hour(data[[i]]$ObsTime[1])))*2*pi/6)
sin3 <- sin(((1:len)-hour(data[[i]]$ObsTime[1]))*2*pi/6)

plot(cos1[1:100],type='l',col=1)
lines(cos2[1:100],col=2)
lines(sin1[1:100],col=3)
lines(sin2[1:100],col=4)

dat <- cbind(Consumption,Temperature,Radiation,sin1,sin2,cos1,cos2,sin3,cos3)
dm <- define.model(kvar=9,ar =c(1,2,3), ma =c(1), reg.var = c(2,3,4,5,6,7,8,9))
dm$ar.pattern[1,c(2,3,4,5,6,7,8,9),c(3,4)]<- 0
m2 <- marima(dat, ar.pattern = dm$ar.pattern, ma.pattern = dm$ma.pattern, Plot = 'log.det',penalty=0)
short.form(m2$ar.estimates)
short.form(m2$ma.estimates)
short.form(m2$ar.stdv)
short.form(m2$ma.stdv)


plot(m2$ar.estimates[1,4,2]*sin1[1:24]+m2$ar.estimates[1,5,2]*sin2[1:24]
     +m2$ar.estimates[1,6,2]*cos1[1:24]+m2$ar.estimates[1,7,2]*cos2[1:24]+m2$ar.estimates[1,8,2]*sin3[1:24]+m2$ar.estimates[1,9,2]*cos3[1:24],type='l',ylab='')
plot(Houravg[,i],type='l')

# marima estimere ma delen p?? en iterativ m??de, normalt skal man bruge kalman filtering
# referer artikel

# AIC og BIC
# BIC :  RMSE + log(p)

