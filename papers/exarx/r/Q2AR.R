##----------------------------------------------------------------
## 2.1 AR(1) with lm
##----------------------------------------------------------------
## Estimate parameters in an AR(1) model using lm
##
rm(list=ls())
setwd(".")
## Source the scripts with functions in the "functions" folder. Just a neat way of arranging helping functions in R
files <- dir("functions", full.names=TRUE)
for(i in 1:length(files)) source(files[i])

## Number of points
n <- 1000
## White noise
eps <- rnorm(n,mean=0,sd=1)
hist(eps)
## Simulate an AR(1) model: Y_t = 0.8*Y_{t-1} + eps_t
y <- filter(eps, filter=0.8, method="recursive")
acf(y)
## Put the y and a one-step lagged y into a data.frame
D <- data.frame(y.l0=y, y.l1=lagVec(y,1))
## Fit the AR(1) model as a linear regression model, the "0 +" indicates no intercept
fit <- lm(y.l0 ~ 0 + y.l1, D)
summary(fit)

## Were the true value included in a 95% confidence band (+- 1.96*Std.Error)?
summary(fit)

## Model validation:
## Plot the residuals
plot(fit$residuals)
## And the ACF of the residuals
acf(fit$residuals)
## Plot the histogram and qq-norm plot
par(mfrow=c(1,2))
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)


##----------------------------------------------------------------
## 2.1 AR(2) with lm
##----------------------------------------------------------------
## Simulate an AR(2) model
## Y_t = 0.6*Y_{t-1} + 0.3*Y_{t-2} + eps_t
y <- filter(rnorm(n,sd=0.3),filter=c(0.6,0.3),method="recursive")

## Plot the series
plot(y)
## Make a data.frame with lagged series
D <- data.frame(y.l0=y, y.l1=lagVec(y,1), y.l2=lagVec(y,2), y.l3=lagVec(y,3))
## Use one lag in the model, AR(1)
fit <- lm(y.l0 ~ 0 + y.l1, D)
## Is the parameter value estimated well?
summary(fit)
## Plot the residuals
plot(fit$residuals)
## And the ACF of the residuals
acf(fit$residuals)

## Use both lags as inputs, AR(2)
fit <- lm(y.l0 ~ 0 + y.l1 + y.l2 , D)
## Are the true values within a 95% confidence band? prm +- 2*Std.Error
summary(fit)
## Plot the residuals
plot(fit$residuals)
## And the ACF of the residuals
acf(fit$residuals)

## Use three lags as inputs, AR(3)
fit <- lm(y.l0 ~ 0 + y.l1 + y.l2 + y.l3, D)
## Are all estimates significantly different from 0?
summary(fit)
## Plot the residuals
plot(fit$residuals)
## And the ACF of the residuals
acf(fit$residuals)
