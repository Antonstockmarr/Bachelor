rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")

# Initializing
s.test <- vector(mode = "list", length = n)
sign.test <- vector(mode = "list", length = n)
lm.simple <- vector(mode = "list", length = n)
model.data <- weatherCons
plotpoints<-matrix(rep(0,3*n),ncol=n)
ts<-matrix(rep(0,n*2),ncol=n)
for (i in c(1:n)) {
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  
  lm.simple[[i]] <- lm(Consumption ~ Temperature, data = model.tmp)
  #print(lm.simple[[i]]$coefficients[1:2])
  f<-summary(lm.simple[[i]])
  f<-f$coefficients[2,1:2]
  plotpoints[,i]<-c(f[1]-2*f[2],f[1],f[1]+2*f[2])
  # Checking model assumptions 
  par(mfrow = c(2,2), mar = c(3,3,3,1) + 0.1)
  plot(lm.simple[[i]])
  title(paste("Daily consumption for house ", i, "using simple lm"), outer=TRUE, adj = 0.5, line = -1.25)
  # Testing for normality
  s.test[[i]] <- shapiro.test(lm.simple[[i]]$residuals)
  ts[1,i]<-(s.test[[i]]$p.value)
  
  sign.test[[i]] <- binom.test(x = sum(sign(lm.simple[[i]]$residuals) == 1), n = length(lm.simple[[i]]$residuals))
  ts[2,i]<-(sign.test[[i]]$p.value)
  # 95% confidence interval
  
  #print(ts[,i])
  
}
par(mfrow=c(1,1))
plot(plotpoints[2,],ylim=c(-10,0))
for(i in 1:n){
  lines(c(i,i),c(plotpoints[1,i],plotpoints[3,i]),col=2)
}
abline(h=0,lty=2,col=Wcol[2])

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
