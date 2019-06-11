### pbs
source('stepP.R')
i=2
wd<-weatherCons[[i]]$WindDirection
wd <- seq(from =0, to = 360, by = 360/1000)
library(pbs)
wd = wd[order(wd)]
#fit
lasse = pbs(wd, df = NULL, knots = c(90,180,270), degree = 1, intercept = T, Boundary.knots = c(0,360))
wd2 <- wd
wd2[wd2<45] <- wd2[wd2<45]+360
lasse = pbs(wd2, df = NULL, knots = c(135, 225,315), degree = 2, intercept = T, Boundary.knots = c(45,405))

plot(wd, lasse[,1], col = 1,ylim=c(0,1),xlim=c(0,360),xaxt = 'n',xlab = 'Direction in degrees',ylab = 'Spline value',main="Spline Basis",type='l',lwd=6)
axis(1, at=seq(0,360, length=9), labels=c(0,45,90,135,180,225,270,315,360))

for (j in 2:4)
{
  lines(wd,lasse[,j], col=j,lwd = 6)
}
abline(v=c(45,135,225,315),lwd=2)
legend(x='topright',legend = c('East','South','West','North'), col = c(4,1,2,3),lty = c(1,1,1,1),cex=1.1,lwd=2)

#points(wd, rowSums(lasse), col = 9)

############

fit<-lm(Consumption ~Temperature+(lasse*WindSpeed), data = weatherCons[[i]])
summary(fit)

plot(-1.59*lasse[,1]+0.42*lasse[,2]+0.19*lasse[,3])

fitsh<-stepP(fit)
fitsh$object
summary(fitsh$object)
#a <- exp(fitsh$object$coefficients[3:6])
a <- c(-0.5,0.8,0.7,0.9)
#a <- c(1,1,1,1)
BSplines <- matrix(data=lasse %*% diag(a),ncol=4)
Knot <- matrix(c(0,1,1,0,0,-1,-1,0),nrow=4,byrow=T)
Spline <- (BSplines)%*%Knot
plot(Spline[,1],Spline[,2],xlim=c(-1,1),ylim=c(-1,1),col=Wcol[2],main = 'Dependency on the wind direction',xlab='West - East',ylab='South - North')
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
