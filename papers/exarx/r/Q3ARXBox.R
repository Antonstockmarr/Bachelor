##----------------------------------------------------------------
## Init
##----------------------------------------------------------------
rm(list=ls())

## Source the scripts with functions in the "functions" folder. Just a neat way of arranging helping functions in R
files <- dir("functions", full.names=TRUE)
for(i in 1:length(files)) source(files[i])

## Read the data and pre-process by resampling to 60 minutes averages and making lagged series
X <- readdatabox()

## See the names of series (columns in the data.frame)
names(X)

## Plot the series of interest
setpar("ts", mfrow=c(4,1))
plot(X$t,X$Qi.l0,type="l")
plot(X$t,X$Ti.l0,type="l")
plot(X$t,X$Te.l0,type="l")
plot(X$t,X$Gv.l0,type="l")
axis.POSIXct(1,X$t,xaxt="s")


##----------------------------------------------------------------
## 3.1 CCF
##----------------------------------------------------------------
graphics.off()
## The cross-correlation (CCF) is the auto-correlation between two series, for example between the heat and the global vertical radiation
ccf(X$Qi,X$Gv)
## Bar at lag zero
cor(X$Qi.l0,X$Gv.l0)
## Bar at lag one
cor(X$Qi.l0,X$Gv.l1)
## Bar at lag two
cor(X$Qi.l0,X$Gv.l2)

## See also for Te
par(mfrow=c(1,2))
## The CCF between the radiator heat and the external temperature
ccf(X$Qi,X[,"Te.l0"],lag.max=100,xlim=c(0,50),ylab="CCF",main="CCF(Qi,Te)")
## The CCF between the radiator heat and the vertical global radiation
ccf(X$Qi,X[,"Gv.l0"],lag.max=100,xlim=c(0,50),ylab="CCF",main="CCF(Qi,Gv)")


##----------------------------------------------------------------
## 3.2 LM and CCF
##----------------------------------------------------------------
## Fit an linear regression model, i.e. no dynamics
fitReg <- lm(Qi.l0 ~ 0 + Ti.l0 + Te.l0 + Gv.l0, X)

## Are the parameters significantly different from zero?
summary(fitReg)

## Analyze the residuals
eps <- fitReg$residuals
## Setup a 1x3 plot
par(mfrow=c(1,3))
## The ACF
acf(eps,lag.max=50,main="ACF(eps)")
## The cross-correlation function (CCF) between the residuals and the external temperature
ccf(eps,X[,"Te.l0"],lag.max=100,xlim=c(0,50),ylab="CCF",main="CCF(eps,Te)")
## The CCF between the residuals and the vertical global radiation
ccf(eps,X[,"Gv.l0"],lag.max=100,xlim=c(0,50),ylab="CCF",main="CCF(eps,Gv)")

## Do time series plots of the residuals and the time series
setpar("ts",mfrow=c(5,1))
plot(X$t,eps,type="l",ylim=c(-30,30))
plot(X$t,X$Ti.l0,ylab="Ti",type="l",col=3,ylim=c(24.3,28.3))
plot(X$t,X$Te.l0,ylab="Te",type="l",col=4)
plot(X$t,X$Gv,type="l")
plot(X$t,X$Qi.l0,type="l",ylim=c(0,110))
lines(X$t,X$Qi.l0-eps,col=2)
axis.POSIXct(1,X$t,xaxt="s")


##----------------------------------------------------------------
## 3.3 Fit an ARX model
##----------------------------------------------------------------
## Change the model formula!
fitLM <- lm(Qi.l0 ~ 0 + Ti.l0 + Te.l0 + Gv.l0, X)
## Are the parameter estimates significantly different from zero?
summary(fitLM)
## ACF and CCF plots implemented in function
acfccfPlot(fitLM, X)
## Time series plots in a function
tsPlots(fitLM, X)

## Use physical knowledge, hence include Qi.l1 and no Ti.l1, from that point try adding a lag of each input and compare
fitARX2 <- lm(Qi.l0 ~ 0 + Qi.l1 + Qi.l2 + Ti.l0 + Te.l0 + Gv.l0, X)
## Are the parameter estimates significantly different from zero?
summary(fitARX2)
## ACF and CCF plots implemented in function
x11()
acfccfPlot(fitARX2, X)
## Time series plots in a function
tsPlots(fitARX2, X)

## Copy the chunk above, add an input, save the fit in a new variable and analyse the residuals
## Keep on until a suitable model is found


##----------------------------------------------------------------
## 3.4 UA and step response
##----------------------------------------------------------------
## The step response for each of the inputs
## Use your best fit
stepResponseARX(fitARX2,X,"Te")
stepResponseARX(fitARX2,X,"Ti") # Actually, this makes not much sense, since no information of the dynamics from Ti to Q is in data (Ti is constant)
stepResponseARX(fitARX2,X,"Gv")

##----------------------------------------------------------------
## Calculate the estimated HLC-value and gA-value for the box based on the estimated steady-state reponse for Ti and Te, for Gv.
## Use your best fit. See the function in the file "r/functions/HLC.Qi.ARX.R"
HLC.Qi.ARX(fitARX2)
