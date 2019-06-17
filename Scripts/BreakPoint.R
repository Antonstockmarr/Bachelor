# Dette script kan kun k?res efter "source("data.R")"

# Initialisere et alpha for hvert hus.
alpha <- rep(0,n)
j=1

# Hvis der er mindst 1 ?rs data, kigger vi p? det hus.
for(k in 1:n){
  if (length(day.data[[k]]$Flow)>=365){
    t <- weatherCons[[k]]$Temperature
    q <- weatherCons[[k]]$Consumption  
    #Vi antager normalfordeling i consumption p? dage over 20 grader, og ser p? resten af data i forhold til denne normalfordeling.
    tmp.mu <- mean(q[t>=20])
    tmp.sd <- sd(q[t>=20])
    
    # Hvis der ikke er en standard deviation bruges data ikke.
    if(tmp.sd!=0){
      
      tmp.sd.dist <-(q-tmp.mu)/tmp.sd
      
      par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
      plot(t,q,xlab='Temperature',ylab='Standard deviations',col=Wcol[2])#,xlim=c(5,25))
      abline(h=tmp.mu+tmp.sd*2)
      mtext(paste("Breakpoint for house number ",k), outer = TRUE, cex = 1.5)
      pct.in.sd <- rep(0,length(min(t):(max(t)-1)))
      
      # For hver grad tjekkes hvilken andel af datapunkterne der ligger indenfor 2 sd fra mean i normalfordelingen.
      for (i in floor(min(t):(max(t)-1))){
        tmp.q <- q[t<=(i+1)]
        tmp.t <- t[t<=(i+1)]
        tmp.q <- tmp.q[tmp.t>i]
        tmp.t <- tmp.t[tmp.t>i]
        
        pct.in.sd[i-round(min(t))+1] <- (sum(tmp.q>=tmp.mu+2*tmp.sd)+sum(tmp.q<=tmp.mu-2*tmp.sd))/length(tmp.q)
        
      }
      alpha[j]<-min(which(pct.in.sd<0.8))+floor(min(t))
      abline(v=alpha[j],col=Wcol[3])
      plot(min(t):(max(t)-1),pct.in.sd,xlab='Temperature',ylab='Proportion outside interval',col=Wcol[2])
      # Alpha for det p?g?ldende hus er den koldeste grad hvor 80% eller mere er indenfor 2 sd i normalfordelingen
      lines(t,rep(0.8,length(t)),col=Wcol[4])
      abline(v=alpha[j],col=Wcol[3])
      j=j+1
      }
  }
}

alpha<-alpha[alpha>0]

par(mfrow=c(1,1))
hist(alpha,main = 'Histogram of the breakpoint values',xlab = 'Temperature')

break.point<-round(as.numeric(quantile(alpha, .25)))
# Breakpointet bestemmes som 25% kvartilet af alphaerne.

abline(v=break.point,col=Wcol[2],lwd=2)

rm(alpha,i,j,pct.in.sd,q,t,tmp.mu,tmp.q,tmp.sd,tmp.sd.dist,tmp.t)

