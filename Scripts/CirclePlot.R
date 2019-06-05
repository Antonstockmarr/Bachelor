CircleCol <- function(splines,modelobject){
  colors=rep(0,length(splines[,1]))
  f<-summary(modelobject)$coefficients
  
  N<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 1]"))
  E<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 2]"))
  S<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 3]"))
  W<-sapply(names(f[,1]),function(x) identical(x,"I(WindSpeed * Splinebasis)[, 4]"))
  
  ncol=Wcol[2]
  if(sum(N)==1){
    p<-f[match(TRUE,N),4]
    if(p<0.05){
      ncol=Wcol[3]
    }
    if(p<0.01){
      ncol=Wcol[4]
    }
  }
  ecol=Wcol[2]
  if(sum(E)==1){
    p<-f[match(TRUE,E),4]
    if(p<0.05){
      ecol=Wcol[3]
    }
    if(p<0.01){
      ecol=Wcol[4]
    }
  }
  scol=Wcol[2]
  if(sum(S)==1){
    p<-f[match(TRUE,S),4]
    if(p<0.05){
      scol=Wcol[3]
    }
    if(p<0.01){
      scol=Wcol[4]
    }
  }
  wcol=Wcol[2]
  if(sum(W)==1){
    p<-f[match(TRUE,W),4]
    if(p<0.05){
      wcol=Wcol[3]
    }
    if(p<0.01){
      wcol=Wcol[4]
    }
  }
  
  colors[splines[,3]==0]<-ncol
  colors[splines[,4]==0]<-ecol
  colors[splines[,1]==0]<-wcol
  colors[splines[,2]==0]<-scol
  return(colors)
}


CirclePlot <- function(WindPred){
  WindPred<-WindPred-min(WindPred)
  x<- -cos((1:360)/180*pi+pi/2)*WindPred$fit
  y<- sin((1:360)/180*pi+pi/2)*WindPred$fit
  CircleCol<-rep(Wcol[3],length(WindPred$fit))
  WindPredC<-WindPred-mean(WindPred$fit)
  CircleCol[WindPredC$upr<0]<-Wcol[2]
  CircleCol[WindPredC$lwr>0]<-Wcol[4]
  plot(x,y,col=CircleCol)
  abline(v=0,h=0)
}

CirclePlot(Wind.Pred[[70]])
