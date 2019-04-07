### pbs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data.R")
source('stepP.R')
i=1
wd<-weatherCons[[i]]$WindDirection
#wd <- c(seq(1,360,length.out=length(weatherCons[[i]]$WindDirection)))
library(pbs)

#fit
lasse = pbs(wd, df = NULL, knots = c(90, 180, 270), degree = 2, intercept=T, 
          Boundary.knots = c(0, 360), periodic = TRUE)

plot(wd, lasse[,1], col = 1,ylim=c(0,1),xlim=c(0,360))
points(wd, lasse[,2], col = 2)
points(wd, lasse[,3], col = 3)
points(wd, lasse[,4], col = 4)

points(wd, rowSums(lasse), col = 9)

############

fit<-lm(Consumption ~Temperature+(lasse*WindSpeed), data = weatherCons[[i]])
summary(fit)

fitsh<-stepP(fit)
fitsh$object
summary(fitsh$object)

a <- c(0.5,0.5,0.7,0.5)
BSplines <- matrix(data=lasse[,1:4] %*% diag(a),ncol=4)
Knot <- matrix(c(1,0,0,-1,-1,0,0,1),nrow=4,byrow=T)
Spline <- (BSplines)%*%Knot
plot(Spline[,1],Spline[,2],xlim=c(-1,1),ylim=c(-1,1),col=Wcol[2],main = 'Dependency on the wind direction',xlab='West - East',ylab='North - South')
abline(h=0,v=0)
##################

model = lm(house.Energy ~ (Temperature + SunHour + WindSpeed + Condition + UltravioletIndex 
                           + MeanSeaLevelPressure + DewPoint + PrecipitationProbability)^2 
           + (WindSpeed + WindDirectionNames)^2 + dayType + vinterBreak 
           + easterBreak + fallBreak + ChristmasBreak, data = modelDFSubset)
tmp = stepP(model)

## save all parameter names
parameterNames<-c(parameterNames,variable.names(t(summary(tmp$object)$coefficient)))

modelListSlope[[i]] = tmp$object #for slopes
modelListPval[[i]] = summary(fitsh$object)$coefficients #for p vals



###########!

plot(1:length(wd), (wd*ttt[,1] + wd *ttt[,2] + wd*ttt[,3] + wd*ttt[,4]))

x = c(1:length(y))
y = wd
plot(x,y, type="l")

summary( fm3 <- lm(y ~ pbs(x, df = 5, Boundary.knots = c(0, 360)))
)
plot(sin(x) + cos(2*3), predict(fm3, data.frame(x=x, z=3)))
summary(sin(x) + cos(2*3)- predict(fm3, data.frame(x=x, z=3)))
