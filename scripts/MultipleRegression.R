rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0),mfrow=c(1,1),xpd=FALSE)

source("data.R")
source("stepP.R")
source("BSplines.R")
source("CircleCol.R")

# Initializing vectors containing "long" and "short" houses
k <-1:n
Long <- k[Datalengths>=360]
Short <- k[Datalengths<360]

# Defining data used for modeling
model.data <- weatherCons
# Various attributes are removed
for (i in 1:n)
{
  model.data[[i]]$Date <- NULL
  model.data[[i]]$PrecipitationProbability <- NULL
  model.data[[i]]$SunHour <- NULL
  model.data[[i]]$UltravioletIndex <- NULL
  model.data[[i]]$Condition <- NULL
}


# Full regression model ---------------------------------------------------
# Initializing
lmMultipleFull <- vector(mode = "list", length = n)
lmFull_est_L <- matrix(rep(0,17*length(Long)),nrow = length(Long))
lmFull_p_L <- matrix(rep(0,17*length(Long)),nrow = length(Long))
lmFull_est_S <- matrix(rep(0,15*length(Short)),nrow = length(Short))
lmFull_p_S <- matrix(rep(0,15*length(Short)),nrow = length(Short))

# Full regression model for "long" houses
for (i in Long) {
  print(paste('Full Model of long house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)

  # Splines
  tmp.wind <- Splinebasis*model.tmp$WindSpeed
  model.tmp$North <- tmp.wind[,3]
  model.tmp$East <- tmp.wind[,4]
  model.tmp$South <- tmp.wind[,1]
  model.tmp$West <- tmp.wind[,2]
  lmMultipleFull[[i]] <- lm(Consumption ~ Temperature*(North + East + South + West)+MeanSeaLevelPressure+Radiation+WinterBreak+SpringBreak+AutumnBreak+ChristmasBreak+Weekend, data = model.tmp)
  
  # Saving coefficients
  lmFull_est_L[match(i,Long),] <- summary(lmMultipleFull[[i]])$coefficients[,1]
  lmFull_p_L[match(i,Long),] <- summary(lmMultipleFull[[i]])$coefficients[,4]
}

# Full regression model for "short" houses
for (i in Short) {
  print(paste('Full Model of Short house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)

  tmp.wind <- Splinebasis*model.tmp$WindSpeed
  model.tmp$North <- tmp.wind[,3]
  model.tmp$East <- tmp.wind[,4]
  model.tmp$South <- tmp.wind[,1]
  model.tmp$West <- tmp.wind[,2]
  lmMultipleFull[[i]] <- lm(Consumption ~ Temperature*(North + East + South + West)+MeanSeaLevelPressure+Radiation+AutumnBreak+ChristmasBreak+Weekend, data = model.tmp)

  lmFull_est_S[match(i,Short),] <- summary(lmMultipleFull[[i]])$coefficients[,1]
  lmFull_p_S[match(i,Short),] <- summary(lmMultipleFull[[i]])$coefficients[,4]
  
}

# Initializing a matrix containing empty strings for "long" houses
lmSummary_star_L <- matrix(rep('',17*length(Long)),nrow = length(Long))
# Adding signs and stars
for(i in 1:length(Long)){
  for(j in 1:17){
    if(lmFull_est_L[i,j]<0){
      lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'-')
    }else{
      lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'+')
    }
    if(lmFull_p_L[i,j]<0.05){
      lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'*')
      if(lmFull_p_L[i,j]<0.01){
        lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'*')
      }
      if(lmFull_p_L[i,j]<0.001){
        lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'*')
      }
    }else if(lmFull_p_L[i,j]<0.1){
      lmSummary_star_L[i,j] <-paste(lmSummary_star_L[i,j],'.')
    }
  }
}
# Printing table 
colnames(lmSummary_star_L) <- c("I","T","N","E","S","W","MeanSeaLvl","SolaR","WinterB","SpringB","AutumnB","ChristB","Weekend","T:N","T:E","T:S","T:W")
write.csv2(lmSummary_star_L, file = "lmMult_star_L.csv", row.names = TRUE)
star_count_L_array <- lmSummary_star_L
star_count_L_array <- gsub("\\.", "", star_count_L_array)
star_count_L_array <- nchar(star_count_L_array)
star_count_L_array <- star_count_L_array>3
colSums(star_count_L_array)

# Initializing a matrix containing empty strings for "short" houses
lmSummary_star_S <- matrix(rep('',15*length(Short)),nrow = length(Short))
for(i in 1:length(Short)){
  for(j in 1:15){
    if(lmFull_est_S[i,j]<0){
      lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'-')
    }else{
      lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'+')
    }
    if(lmFull_p_S[i,j]<0.05){
      lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'*')
      if(lmFull_p_S[i,j]<0.01){
        lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'*')
      }
      if(lmFull_p_S[i,j]<0.001){
        lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'*')
      }
    }else if(lmFull_p_S[i,j]<0.1){
      lmSummary_star_S[i,j] <-paste(lmSummary_star_S[i,j],'.')
    }
  }
}
colnames(lmSummary_star_S) <- c("I","T","N","E","S","W","MeanSeaLvl","SolaR","AutumnB","ChristB","Weekend","T:N","T:E","T:S","T:W")
write.csv2(lmSummary_star_S, file = "lmMult_star_S.csv", row.names = TRUE)
star_count_S_array <- lmSummary_star_S
star_count_S_array <- gsub("\\.", "", star_count_S_array)
star_count_S_array <- nchar(star_count_S_array)
star_count_S_array <- star_count_S_array>3
colSums(star_count_S_array)
#summary(stepP(lmMultipleFull[[6]])$object)
#summary(lmMultipleFull[[6]])


# General regression model for comparing houses ---------------------------
lmMultiple <- vector(mode="list", length = n)
lmMultipleNoP <- vector(mode = "list", length = n)
lmSummary_est <- matrix(rep(0,11*n),nrow = n)
lmSummary_p <- matrix(rep(0,11*n),nrow = n)
Wind.Pred <- vector(mode = "list", length = n)
colnames(lmSummary_est) <- c("I","T","N","E","S","W","SolaR","T:N","T:E","T:S","T:W")
colnames(lmSummary_p) <- c("I","T","N","E","S","W","SolaR","T:N","T:E","T:S","T:W")


par(mfrow = c(1,1))
for (i in 1:n) {
  print(paste('Modeling house ',i))
  model.tmp <- model.data[[i]]
  model.tmp <- model.tmp[model.tmp$Temperature <= 12,]
  Splinebasis <- BSplines(model.tmp$WindDirection)

#  wd <- model.tmp$WindDirection[order(model.tmp$WindDirection)]
#  wd[wd<45] <- wd[wd<45]+360
  tmp.wind <- Splinebasis*model.tmp$WindSpeed#[order(wd)]
#  tmp.wind <- model.tmp$WindSpeed[order(model.tmp$WindDirection)]
  model.tmp$North <- tmp.wind[,3]
  model.tmp$East <- tmp.wind[,4]
  model.tmp$South <- tmp.wind[,1]
  model.tmp$West <- tmp.wind[,2]
  lmMultipleNoP[[i]] <- lm(Consumption ~ Temperature*(North + East + South + West)+
                                                        Radiation, data = model.tmp)
  lmMultiple[[i]] <- stepP(lmMultipleNoP[[i]])
  # Checking model assumptions 
  par(mfrow = c(2,2), mar = c(3,3,3,1) + 0.1)
  plot(lmMultiple[[i]]$object)
  title(paste("Daily consumption for house ", i), outer=TRUE, adj = 0.5, line = -1.25)


  lmSummary_est[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,1]
  lmSummary_p[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,4]

  
  # Wind profile plot
  par(mfrow = c(1,1))
  model.Wind<-data.frame(Consumption=model.tmp$Consumption,Temperature=model.tmp$Temperature,Radiation=model.tmp$Radiation,N=model.tmp$North,E=model.tmp$East,S=model.tmp$South,W=model.tmp$West)
  lmMultipleNoP[[i]] <- lm(Consumption ~ .+Temperature*(N + E + S + W),data = model.Wind)
  Splinebasis2 <- BSplines(1:360)
  newData = data.frame(Temperature = rep(0, 360), # 0 grader
                       Radiation = rep(0, 360), # Om natten
                       N = Splinebasis2[,3],
                       E = Splinebasis2[,4],
                       S = Splinebasis2[,1],
                       W = Splinebasis2[,2])
  
  Wind.Pred[[i]]<-data.frame(predict(object=lmMultipleNoP[[i]], newdata=newData, interval = "confidence", level = 0.95))
  
  plot(Wind.Pred[[i]]$fit,type='l',ylim=range(Wind.Pred[[i]]$lwr,Wind.Pred[[i]]$upr),main=paste("hus: ",i))
  lines(Wind.Pred[[i]]$upr,lty=2)
  lines(Wind.Pred[[i]]$lwr,lty=2)
  abline(v=c(0,90,180,270,360), col="gray", lty=2, lwd=1)
  
}

t.est <- as.table(lmSummary_est)
# Saving estimates in a .csv file
write.csv2(t.est, file = "lmMult_est.csv", row.names = TRUE)
t.pvalues <- as.table(lmSummary_p)
# Saving p-values in a .csv file
write.csv2(t.pvalues, file = "lmMult_pvalues.csv", row.names = TRUE)


# Making +*** table
lmSummary_star <- matrix(rep('',12*n),nrow = n)
for(i in 1:n){
  lmSummary_star[i,1]<-i
  for(j in 2:12){
    if(lmSummary_est[i,j-1]<0){
      lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'-')
    }else{
      lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'+')
    }
    if(lmSummary_p[i,j-1]<0.05){
      lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'*')
      if(lmSummary_p[i,j-1]<0.01){
        lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'*')
      }
      if(lmSummary_p[i,j-1]<0.001){
        lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'*')
      }
    }else if(lmSummary_p[i,j-1]<0.1){
      lmSummary_star[i,j] <-paste(lmSummary_star[i,j],'.')
    }
  }
}

colnames(lmSummary_star) <- c("HouseIndex","I","T","N","E","S","W","SolaR","T:N","T:E","T:S","T:W")
write.csv2(lmSummary_star, file = "lmMult_star.csv", row.names = TRUE)
write.csv2(lmSummary_star[Long,], file = "lmMult_L_star.csv", row.names = TRUE)
write.csv2(lmSummary_star[Short,], file = "lmMult_S_star.csv", row.names = TRUE)
star_count_array <- lmSummary_star
star_count_array <- gsub("\\.", "", star_count_array)
star_count_array <- nchar(star_count_array)
star_count_array <- star_count_array>3
colSums(star_count_array)/n



# Counting negative estimates
sum(lmSummary_est[,3:6] < 0) / sum(lmSummary_est[,3:6] < 1000000)
sum(lmSummary_est[,8:11] < 0) / sum(lmSummary_est[,8:11] < 1000000)


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
