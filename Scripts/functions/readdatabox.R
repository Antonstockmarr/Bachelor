readdatabox <- function(sp=60,nlags=6)
    {
        ## Read data, X
        load("databox.rda")

        ## Resample, note time points are in the end of the avereging periods
        X <- resampleDF(X,sp*60,startTime=trunc(X$t[1],unit="hours"))
        X$tod <- as.double(X$t - trunc(X$t,"day"), unit="hours")

        ## Make lagged versions of the inputs
        ## Heat input
        X[,paste0("Qi.l",0:nlags)] <- lagMat(X$Qi, lag=0:nlags)
        ## Internal air temperature
        X$Ti <- (X$Ti.down+X$Ti.up)/2
        X[,paste0("Ti.l",0:nlags)] <- lagMat(X$Ti, lag=0:nlags)
        ## External air temperature
        X[,paste0("Te.l",0:nlags)] <- lagMat(X$Te, lag=0:nlags)
        ## Global vertical radiation on surface facing south
        X[,paste0("G.l",0:nlags)] <- lagMat(X$G, lag=0:nlags)
        ## Wind speed
        X[,paste0("Ws.l",0:nlags)] <- lagMat(X$WV, lag=0:nlags)
        ## Wind direction
        X[,paste0("Wd.l",0:nlags)] <- lagMat(X$WD, lag=0:nlags)
        ## Wind speed time external temperature
        X[,paste0("WsTe.l",0:nlags)] <- lagMat(X$WV*X$Te.l0, lag=0:nlags)

        ## Remove the first nlags values, they are NA
        X <- X[-(1:nlags),]
        
        ## Return X
        invisible(X)
    }
