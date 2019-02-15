rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))


## Loading data
dat <- read.csv('../DistrictHeatingTestSample.csv', header=TRUE, sep = "\t")
head(dat)
str(dat)

## Loading all data
data.path = "../meterdata/"

