## First data analysis -  Bachelor project

setwd("/users/anton/Google\ Drev/Skolerelateret/DTU/Bachelor\ Project/Data")

dat = read.csv("DistrictHeatingTestSample.csv", header = TRUE, sep = ';')

plot(dat$TemperatureIn,col='red',type='l',ylim=c(20,80))
lines(dat$TemperatureOut,col='blue')
