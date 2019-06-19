rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0),mfrow=c(1,1),xpd=FALSE)

source("data.R")
source("stepP.R")
source("BSplines.R")
source("CirclePlot.R")

# Initializing vectors containing "long" and "short" houses
k <-1:n
Long <- k[Datalengths>=360]
Short <- k[Datalengths<360]

#Flip WeatherCons
for(i in 1:n){
  k<-dim(weatherCons[[i]])[1]
  weatherCons[[i]]<-weatherCons[[i]][k:1,]
}

# Defining data used for modelling
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
sMultiple.test <- vector(mode = "list", length = n)
sign.testM <- vector(mode = "list", length = n)
t<-matrix(rep(0,n*2),ncol=n)
par(mfrow = c(1,1))
for (i in c(18,55)) {
  print(paste('Modelling house ',i))
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
  # Checking model assumptions 
  par(mfrow = c(2,2), mar = c(3,3,3,1) + 0.1)
  plot(lmMultipleNoP[[i]])
  title(paste("Daily consumption for house ", i), outer=TRUE, adj = 0.5, line = -1.25)
  # Testing for normality
  sMultiple.test[[i]] <- shapiro.test(lmMultipleNoP[[i]]$residuals)
  t[1,i]<-(sMultiple.test[[i]]$p.value)
  sign.testM[[i]] <- binom.test(x = sum(sign(lmMultipleNoP[[i]]$residuals) == 1), n = length(lmMultipleNoP[[i]]$residuals))
  t[2,i]<-(sign.testM[[i]]$p.value)

  lmSummary_est[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,1]
  lmSummary_p[i,] <- summary(lmMultipleNoP[[i]])$coefficients[,4]

  
  # Wind profile plot
  par(mfrow = c(1,1))
  model.Wind<-data.frame(Consumption=model.tmp$Consumption,Temperature=model.tmp$Temperature,Radiation=model.tmp$Radiation,N=model.tmp$North,E=model.tmp$East,S=model.tmp$South,W=model.tmp$West)
  lmMultipleNoP[[i]] <- lm(Consumption ~ .+Temperature*(N + E + S + W),data = model.Wind)
  Splinebasis2 <- BSplines(0:359)
  newData = data.frame(Temperature = rep(0, 360), # 0 grader
                       Radiation = rep(0, 360), # Om natten
                       N = Splinebasis2[,3]*4.27, # Gennemsnitlig vindstyrke
                       E = Splinebasis2[,4]*4.27,
                       S = Splinebasis2[,1]*4.27,
                       W = Splinebasis2[,2]*4.27)

  Wind.Pred[[i]]<-data.frame(predict(object=lmMultipleNoP[[i]], newdata=newData, interval = "confidence", level = 0.95))
  Wind.PredK<-data.frame(predict(object=lmMultipleNoP[[i]], newdata=newData, interval = "confidence", level = 0.33))

  plot(Wind.Pred[[i]]$fit,type='l',ylim=range(Wind.Pred[[i]]$lwr,Wind.Pred[[i]]$upr),main=paste("House: ",i),xlab = "Wind direction in degrees",ylab="Effect on consumption")
  lines(Wind.Pred[[i]]$upr,lty=2)
  lines(Wind.Pred[[i]]$lwr,lty=2)
  abline(v=c(0,90,180,270,360), col="gray", lty=2, lwd=1)

  CirclePlot(Wind.PredK)
  title(main=paste("House: ",i))
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


# Plotting coefficients
library('ggplot2')

coef<-data.frame(ID=paste(1:n),Slope=rep(0,n),Lower=rep(0,n),Upper=rep(0,n))

for(i in 1:n){
  lmsum <- summary(lmMultipleNoP[[i]])
  coef$Slope[i] <- lmsum$coefficients[2,1]/BBR$TotalArea[i]
  coef$Lower[i] <- (lmsum$coefficients[2,1]-2*lmsum$coefficients[2,2])/BBR$TotalArea[i]
  coef$Upper[i] <- (lmsum$coefficients[2,1]+2*lmsum$coefficients[2,2])/BBR$TotalArea[i]
}
coef<-coef[!is.na(coef$Slope),]

plot.index <- order(coef$Slope, decreasing = TRUE)
coef <- coef[plot.index,]

plotgg1 <- ggplot(coef[plot.index,]) +
  geom_bar(aes(x = reorder(ID,-Slope,sum), y = Slope), stat = 'identity', fill = 'cornflowerblue', color = 'black') + 
  geom_errorbar(aes(x = ID, ymin = Lower, ymax = Upper), width = 0.4, color = 'orangered') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90,hjust = 0.5)) +
  xlab('Building no.') +
  ylab(expression(paste("Temp. coefficients [kWh/(",degree,"C",~ m^2 ~ day,")]", sep="")))

  {
  pdf(file = "../figures/Temp_coef.pdf",width = 8.6,height = 4.3,pointsize = 9)
  print(plotgg1)
  dev.off()
  }


# Year of construction plot
# The last year of construction is saved for a plot.
coef$Construction.Year <- 1:length(coef$ID)
tmp<-as.numeric(as.character(coef$ID))
for(i in 1:length(coef$ID)){
  if(!is.na(BBR$ReconstructionYear[tmp[i]])){
    coef$Construction.Year[i]<-BBR$ReconstructionYear[tmp[i]]
  }else{
    coef$Construction.Year[i]<-BBR$ConstructionYear[tmp[i]]
  }
}
plot(coef$Construction.Year,coef$Slope)
for(i in 1:length(coef$ID)){
  lines(c(coef$Construction.Year[i],coef$Construction.Year[i]),c(coef$Lower[i],coef$Upper[i]),col=2)
}

# Solar
coef<-data.frame(ID=paste(1:n),Slope=rep(0,n),Lower=rep(0,n),Upper=rep(0,n))

for(i in 1:n){
  lmsum <- summary(lmMultipleNoP[[i]])
  coef$Slope[i] <- lmsum$coefficients[3,1]
  coef$Lower[i] <- (lmsum$coefficients[3,1]-2*lmsum$coefficients[3,2])
  coef$Upper[i] <- (lmsum$coefficients[3,1]+2*lmsum$coefficients[3,2])
}
coef<-coef[!is.na(coef$Slope),]

plot.index <- order(coef$Slope, decreasing = TRUE)
coef <- coef[plot.index,]

plotgg2 <- ggplot(coef[plot.index,]) +
  geom_bar(aes(x = reorder(ID,-Slope,sum), y = Slope), stat = 'identity', fill = 'cornflowerblue', color = 'black') + 
  geom_errorbar(aes(x = ID, ymin = Lower, ymax = Upper), width = 0.4, color = 'orangered') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90,hjust = 0.5)) +
  xlab('Building no.') +
  ylab(expression(paste("Solar rad. coefficients [",kWh,"]")))

{
  pdf(file = "../figures/Solar_coef.pdf",width = 8.6,height = 4.3,pointsize = 9)
  print(plotgg2)
  dev.off()
}



