rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

model.data <- weatherCons
for (i in 55:55) {
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  
  lm.simple <- lm(Consumption ~ Temperature, data = model.tmp)
  summary(lm.simple)
  # Checking model assumptions 
  par(mfrow = c(2,2), mar = c(3,3,3,1) + 0.1)
  plot(lm.simple)
  title(paste("Daily consumption for house ", i), outer=TRUE, adj = 0.5, line = -1.25)
}
plot(Consumption ~ Temperature, data = model.tmp)
abline(lm.simple)

acf(lm.simple$residuals)
