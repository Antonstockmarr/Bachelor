rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))

source("data.R")
source("stepP.R")
source("BSplines.R")
source("CircleCol.R")

# Defining new data set where the summer period is left out
model.data <- weatherCons
# Various attributes are removed 
for (i in 1:n)
{
  model.data[[i]]$Date <- NULL
  model.data[[i]]$PrecipitationProbability <- NULL
  model.data[[i]]$SunHour <- NULL
  model.data[[i]]$UltravioletIndex <- NULL
}


# Full regression model ---------------------------------------------------
lmMultiple <- vector(mode="list", length = n)
lmMultipleNoP <- vector(mode = "list", length = n)
lmSummary_est <- matrix(rep(0,11*n),nrow = n)
lmSummary_p <- matrix(rep(0,11*n),nrow = n)

for (i in 1:n) {
  
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)
  lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(I(WindSpeed*Splinebasis)[,1]+
                                                        I(WindSpeed*Splinebasis)[,2]+
                                                        I(WindSpeed*Splinebasis)[,3]+
                                                        I(WindSpeed*Splinebasis)[,4])+
                                                        Radiation, data = model.tmp)
  lmMultiple[[i]] <- stepP(lmMultipleNoP[[i]])
  
  
  
  
  BSplin <- matrix(data=Splinebasis %*% diag(4),ncol=4)
  Knot <- matrix(c(0,1,1,0,0,-1,-1,0),nrow=4,byrow=T)
  Spline <- (BSplin)%*%Knot
  plot(Spline[,1],Spline[,2],xlim=c(-1,1),ylim=c(-1,1),col=CircleCol(Splinebasis,lmMultiple[[i]]$object),main = paste('Dependency on the wind direction for house ',i),xlab='West - East',ylab='South - North')
  abline(h=0,v=0)
  
  lmSummary_est[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,1] 
  lmSummary_p[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,4] 
  
}
colnames(lmSummary_est) <- c("I","T","W1","W2","W3","W4","SolaR","T:W1","T:W2","T:W3","T:W4")
t.est <- as.table(lmSummary_est)
# Saving estimates in a .csv file 
write.csv(t.est, file = "lmMult_est.csv", row.names = TRUE)
colnames(lmSummary_p) <- c("I","T","W1","W2","W3","W4","SolaR","T:W1","T:W2","T:W3","T:W4")
t.pvalues <- as.table(lmSummary_p)
# Saving p-values in a .csv file 
write.csv(t.pvalues, file = "lmMult_pvalues.csv", row.names = TRUE)


# Investigating parameters from model
summary(lmMultipleNoP[[1]])
par(mfrow=c(2,2))
# Checking model assumptions 
for (i in 1:n)
{
  plot(lmMultiple[[i]]$object)
}

# Saving slopes and p-values
modelListSlope <- vector(mode="list",length=n)
modelListPval <- vector(mode="list",length=n)

for (i in 1:n)
{
  modelListSlope[[i]] = lmMultiple[[i]]$object #for slopes
  modelListPval[[i]] = summary(lmMultiple[[i]]$object)$coefficients #for p vals
}


# General regression model for comparing houses ---------------------------

