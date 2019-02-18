rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

test.data <- data[[1]]
test.data$EndDateTime <- NULL
test.data$StartDateTime <- 1:length(test.data$StartDateTime)

pairs(test.data)

plot(test.data$Flow[test.data$Flow<0.4] ~ test.data$StartDateTime[test.data$Flow<0.4])
