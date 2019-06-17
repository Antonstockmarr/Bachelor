tsPlots <- function(fit,X)
    {
      ## Analyze the residuals
      eps <- c(fit$residuals, rep(NA, nrow(X)-length(fit$residuals)))
        ## Do time series plots of the residuals and the time series
        parPre <- setpar("ts",mfrow=c(5,1))
        plot(X$t,eps,type="l",ylim=c(-30,30))
        ##
        plot(X$t,X$Ti.l0,ylab="Ti",type="l",col=3,ylim=c(24.3,28.3))
        ##
        plot(X$t,X$Te.l0,ylab="Ta",type="l",col=4)
        ##
        plot(X$t,X$G,type="l")
        ##
        plot(X$t,X$Qi.l0,type="l",ylim=c(0,110))
        lines(X$t,X$Qi.l0-eps,col=2)
        axis.POSIXct(1,X$t,xaxt="s")
        ## Restore the plotting window setup
        par(parPre)
    }
