rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

pairs(data[[1]])

plot(data[[1]]$Flow)

# Investigating each house's flow behaviour 
plot.times = c(df.temp$StartDateTime)
plot.months = c("January", "February", "March", "April", "May", "June", "July", "August", 
                "September", "November", "December")

plot(data[[1]]$StartDateTime, data[[1]]$Flow, type = "l", xlab ="Time", ylab = "Flow of house 1")
axis(1, labels = plot.months, las = 2)
