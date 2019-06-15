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
## 4.1 ARMAX
##----------------------------------------------------------------
#install.packages('marima')
require(marima)

## We need to shift the inputs one lag forward to make it work
X$Gv <- lagVec(X$Gv, -1)
X$Ti <- lagVec(X$Ti, -1)
X$Te <- lagVec(X$Te, -1)
## Fit the ARMAX
fit <- estimateARMAX(outName='Qi', inNames=c('Ti','Te','Gv'), pAR=1, pMA=1)

## Are the parameter estimates significantly different from zero?
summary(fit)
## ACF and CCF plots implemented in function
acfccfPlot(fit, X)
plot(fit$residuals)
## Time series plots in a function
tsPlots(fit, X)

## Try a higher order, i.e. set pAR to 2


##----------------------------------------------------------------
## 3.4 HLC and step response
##----------------------------------------------------------------
## The step response for each of the inputs
## Use your best fit
stepResponseARX(fit,X,"Te")
stepResponseARX(fit,X,"Ti") # Actually, this makes not much sense, since no information of the dynamics from Ti to Q is in data (Ti is constant)
stepResponseARX(fit,X,"Gv")

##----------------------------------------------------------------
## Calculate the estimated HLC-value and gA-value for the box based on the estimated steady-state reponse for Ti and Te, for Gv.
## Use your best fit. See the function in the file "r/functions/HLC.Qi.ARX.R"
HLC.Qi.ARX(fit)

