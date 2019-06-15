## Init
rm(list=ls())
setwd(".")
## Source the scripts with functions in the "functions" folder. Just a neat way of arranging helping functions in R
files <- dir("functions", full.names=TRUE)
for(i in 1:length(files)) source(files[i])

##----------------------------------------------------------------
## 1.1 White noise
##----------------------------------------------------------------
## Make some white noise!
## Number of points
n <- 1000
## Independent identically distributed (i.i.d.) normal distributed numbers: Eps ~ N(mean,sigma^2)
eps <- rnorm(n, mean=0, sd=1) # Standard normal distribution eps ~ N(0,1)
## x is now a (simulated) realization of the stochastic variable X. Plot it
plot(eps)
## The mean of the realization
mean(eps)
## The variance of the realization
var(eps)
## See the distribution
hist(eps)

## See the scatter plot of x_t versus the one-step lagged (shifted) x_{t-1}
plot(eps[1:(length(eps)-1)], eps[2:length(eps)])
## Lagging is implemented in a function "functions/lagVec.R"
## Calculate the correlation between x_t and x_{t-1}
cor(eps, lagVec(eps,1), use="complete.obs")
cor(eps, lagVec(eps,2), use="complete.obs")
## The auto-correlation function (ACF)
acf(eps)
## Verify that bars are simply the correlation between lagged x


##----------------------------------------------------------------
## 1.2 ACF of an AR(1) process
##----------------------------------------------------------------
## ACF of a low-pass filtered noise signal, see ?filter. It is then a simulation of an AR(1) process:
## Y_t = 0.8*Y_{t-1} + eps_t
y <- numeric(length(eps))
y[1] <- eps[1]
for(i in 2:length(eps))
  {
    y[i] <- 0.8*y[i-1] + eps[i]
  }
## Or use the build in function
## y <- filter(eps,filter=0.8,method="recursive")
## Plot
plot(y)
## The ACF
acf(y,lag.max=100)
## Play around with the filter coefficient, i.e. set filter=0.8 to a different value and see how it affects the ACF.


##----------------------------------------------------------------
## 1.3 More ACF
##----------------------------------------------------------------
## ACF of a sine
w <- 10 # frequency
xsin <- sin((1:n)*2*pi*(w/1000))
## Plot it
plot(xsin)
## The ACF is actually also a sine!
acf(xsin,lag.max=100)
## Verify that bars are simply the correlation between lagged x
plot(xsin,lagVec(xsin,1))
cor(xsin,lagVec(xsin,1),use="complete.obs")

## See that the ACF is a relative measure, i.e. you can multiply and it doesn't change
acf(y*1000,lag.max=100)

## What happens to the ACF when random "spikes" or outliers are added to the series?
y2 <- filter(eps,filter=0.8,method="recursive")
## Put in some spikes at random positions, set the level of the spikes here
y2[round(runif(10,1,n))] <- 10
## Plot the series
plot(y2)
## ACF
acf(y2,lag.max=100)
## Plot versus the one-step lagged
plot(y2,lagVec(y2,1))


##----------------------------------------------------------------
## 1.4 ACF game
##----------------------------------------------------------------
## Note that this exercise is just for fun :-) Actually, the ACF should not used for discrete values.
## Type in at least 30 numbers between 1 and 9 and try to see the ACF
## Run the following line and then type in numbers in the Console and press enter
x <- readline()
## Convert the string x to numbers
x <- as.numeric(sapply(1:nchar(x),function(i){substr(x,i,i)}))
## Plot
plot(x)
## Plot x versus one-step lagged x
plot(x[1:(length(x)-1)],x[2:length(x)])
## Plot ACF
acf(x)
## Play around and figure out how to get different patterns in the ACF
