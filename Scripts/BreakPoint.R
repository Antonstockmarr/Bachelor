# This script is only runable after running "source("data.R")"

# Initialize alpha for every house.
alpha <- rep(0,n)
j=1

# The data is used for the houses with at least a year of data.
for(k in 1:n){
  if (length(day.data[[k]]$Flow)>=365){
    t <- weatherCons[[k]]$Temperature
    q <- weatherCons[[k]]$Consumption  
    # We assume that the consumption data for the days with 20+ degrees follows a normal destribution. We compare the rest of the data to this destribution.
    tmp.mu <- mean(q[t>=20])
    tmp.sd <- sd(q[t>=20])
    
    # If the standard deviation id 0, the data is not used.
    if(tmp.sd!=0){
      

      pct.in.sd <- rep(0,length(min(t):(max(t)-1)))
      
      # For every degree, the fraction of datapoints laying inside the 95% CI of the normal destribution is calculated.
      for (i in floor(min(t):(max(t)-1))){
        tmp.q <- q[t<=(i+1)]
        tmp.t <- t[t<=(i+1)]
        tmp.q <- tmp.q[tmp.t>i]
        tmp.t <- tmp.t[tmp.t>i]
        
        pct.in.sd[i-round(min(t))+1] <- (sum(tmp.q>=tmp.mu+1.96*tmp.sd)+sum(tmp.q<=tmp.mu-1.96*tmp.sd))/length(tmp.q)
        
      }
      alpha[j]<-min(which(pct.in.sd<0.8))+floor(min(t))
      
      
      par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
      colour <- rep(Wcol[2],length(t))
      colour[t>=alpha[j]]<-Wcol[4]
      plot(t,q,xlab=expression(paste('Temperature [',degree, 'C]')),ylab='Daily consumption [kWh]',col=colour)#,xlim=c(5,25))
      abline(h=tmp.mu+tmp.sd*1.96,col=Wcol[1])
      mtext(paste("Threshold for house ",k), outer = TRUE, cex = 1.5)
      abline(v=alpha[j],col=Wcol[3])
      colour <- rep(Wcol[2],length(pct.in.sd))
      colour[pct.in.sd <= 0.8] <- Wcol[4]
      plot(min(t):(max(t)-1),pct.in.sd,xlab=expression(paste('Temperature [',degree, 'C]')),ylab='Proportion outside interval',col=colour)
      # Alpha for det p?g?ldende hus er den koldeste grad hvor 80% eller mere er indenfor 2 sd i normalfordelingen
      abline(h=0.8,col=Wcol[4])
      abline(v=alpha[j],col=Wcol[3])
      j=j+1
      }
  }
}

alpha<-alpha[alpha>0]

par(mfrow=c(1,1))
hist(alpha,main = 'Histogram of the threshold values',xlab = expression(paste('Temperature [',degree, 'C]')))

break.point<-round(as.numeric(quantile(alpha, .25)))
# The break point i set to the 25% quantile of the alphas.

abline(v=break.point,col=Wcol[2],lwd=2)

rm(alpha,i,j,pct.in.sd,q,t,tmp.mu,tmp.q,tmp.sd,tmp.t)

