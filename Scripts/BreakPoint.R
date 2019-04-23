# Dette script kan kun k?res efter "source("data.R")"

alpha <- rep(0,n)
j=1

for(k in 1:n){
  if (length(day.data[[k]]$Flow)>=365){
    day.tmp <- day.weather[(day.weather$Date <= as.Date(EndDays[k],tz="GMT")),]
    day.tmp <- day.tmp[day.tmp$Date >= as.Date(StartDays[k],tz="GMT"),] 
    t <- day.tmp$Temperature
    q <- day.data[[k]]$CoolingDegree*day.data[[k]]$Volume
    
    tmp.mu <- mean(q[t>=20])
    tmp.sd <- sd(q[t>=20])
    
    if(tmp.sd!=0){
      
      tmp.sd.dist <-(q-tmp.mu)/tmp.sd
      
      par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
      plot(t,tmp.sd.dist,xlab='Temperature',ylab='Standard deviations',col=Wcol[2])#,xlim=c(5,25))
      lines(t,rep((2),length(t)),col=Wcol[4])
      lines(t,rep((2),length(t)),col=Wcol[4])
      mtext(paste("Breakpoint for house number ",k), outer = TRUE, cex = 1.5)
      pct.in.sd <- rep(0,length(min(t):(max(t)-1)))
      
      for (i in floor(min(t):(max(t)-1))){
        tmp.q <- q[t<=(i+1)]
        tmp.t <- t[t<=(i+1)]
        tmp.q <- tmp.q[tmp.t>i]
        tmp.t <- tmp.t[tmp.t>i]
        
        pct.in.sd[i-round(min(t))+1] <- (sum(tmp.q>=tmp.mu+2*tmp.sd)+sum(tmp.q<=tmp.mu-2*tmp.sd))/length(tmp.q)
        
      }
      alpha[j]<-min(which(pct.in.sd<0.8))+floor(min(t))
      plot(min(t):(max(t)-1),pct.in.sd,xlab='Temperature',ylab='Proportion outside interval',col=Wcol[2])
      lines(t,rep(0.8,length(t)),col=Wcol[4])
      abline(v=alpha[j],col=Wcol[3])
      j=j+1
      }
  }
}

alpha<-alpha[alpha>0]

hist(alpha)

break.point<-round(as.numeric(quantile(alpha, .25)))

rm(alpha,i,j,pct.in.sd,q,t,tmp.mu,tmp.q,tmp.sd,tmp.sd.dist,tmp.t)
