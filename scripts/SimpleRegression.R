rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

# Initializing
s.test <- vector(mode = "list", length = n)
sign.test <- vector(mode = "list", length = n)
lm.simple <- vector(mode = "list", length = n)
model.data <- weatherCons
for (i in 1:n) {
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  
  lm.simple[[i]] <- lm(Consumption ~ Temperature, data = model.tmp)
  #print(summary(lm.simple[[i]]))
  
  # Checking model assumptions 
  par(mfrow = c(2,2), mar = c(3,3,3,1) + 0.1)
  plot(lm.simple[[i]])
  title(paste("Daily consumption for house ", i, "using simple lm"), outer=TRUE, adj = 0.5, line = -1.25)
  # Testing for normality
  s.test[[i]] <- shapiro.test(lm.simple[[i]]$residuals)
  print(s.test[[i]]$p.value)
  
  sign.test[[i]] <- binom.test(x = sum(sign(lm.simple[[i]]$residuals) == 1), n = length(lm.simple[[i]]$residuals))
  print(sign.test[[i]]$p.value)
}

# Calculating p-values under 0.05 for normality tests
Sum_shapiro <- 0
Sum_sign <- 0
for (i in 1:n) {
  if (s.test[[i]]$p.value < 0.05) {
    Sum_shapiro <- Sum_shapiro+1
  }
  if (sign.test[[i]]$p.value < 0.05) {
    Sum_sign <- Sum_sign+1
  }
}

plot(Consumption ~ Temperature, data = model.tmp)
abline(lm.simple)

acf(lm.simple$residuals)
