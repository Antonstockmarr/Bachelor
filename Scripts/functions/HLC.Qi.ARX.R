HLC.Qi.ARX <- function(fit,GName="G\\."){
    ## Extracts estimate of HLC and g values from an estimate of an ARX
    ## model with heat as model output.

    ##  browser()
    ## Calculating the HLC and V(U) using notation according to Jimenez 2008.
    ncoef <- names(fit$coefficients)
    np <- length(fit$coefficients)
    idx.Ti <- which(substr(ncoef,1,3)=="Ti.")
    idx.Te <- which(substr(ncoef,1,3)=="Te.")
    idx.Qi <- which(substr(ncoef,1,3)=="Qi.")
    ## For global radiation
    idx.G <- grep(GName,ncoef)
    ##  idx.G <- which(substr(ncoef,1,3)=="G.")

    ## The polynomials with a 1 through
    A.1 <- -sum(fit$coefficients[idx.Ti])
    B.1 <- sum(fit$coefficients[idx.Te])
    B.2 <- sum(c(-1,fit$coefficients[idx.Qi]))
    B.3 <- sum(c(fit$coefficients[idx.G]))
    
    U1Jime <- A.1/B.2
    U2Jime <- B.1/B.2

    jac.us.s <- matrix(0,nrow=2,ncol=np)
    jac.us.s[1,idx.Ti] <- -B.2
    jac.us.s[1,idx.Te] <- 0
    jac.us.s[1,idx.Qi] <- -A.1
    jac.us.s[2,idx.Ti] <- 0
    jac.us.s[2,idx.Te] <- B.2
    jac.us.s[2,idx.Qi] <- -B.1
    jac.usJime <- jac.us.s/(B.2^2)
    ##print(jac.usJime)
    
    ## Calculate according to the derivation
    ThetaQ1 <- sum(c(1,-fit$coefficients[idx.Qi]))
    ThetaTi1 <- sum(fit$coefficients[idx.Ti])
    ThetaTe1 <- sum(fit$coefficients[idx.Te])
    ThetaG1 <- sum(c(fit$coefficients[idx.G]))

    U1 <- ThetaTi1/ThetaQ1
    U2 <- -ThetaTe1/ThetaQ1

    ## The Jacobian
    jac.us <- matrix(0,nrow=2,ncol=np)
    jac.us[1,idx.Qi] <- ThetaTi1/ThetaQ1^2
    jac.us[1,idx.Ti] <- 1/ThetaQ1
    jac.us[1,idx.Te] <- 0
    jac.us[2,idx.Qi] <- -ThetaTe1/ThetaQ1^2
    jac.us[2,idx.Ti] <- 0
    jac.us[2,idx.Te] <- -1/ThetaQ1
    ##print(jac.us)

    jac.g.s <- matrix(0,nrow=1,ncol=np)
    jac.g.s[1,idx.G] <- B.2
    jac.g.s[1,idx.Qi] <- -B.3

    jac.g <- jac.g.s/B.2^2

    ## From here the same is carried out for each derivation
    ##print("The Covariance matrix of the U values")
    ina <- which(is.na(vcov(fit)[1, ]))
    vcv <- vcov(fit)

    if(length(ina) > 0){
        jac.us <- jac.us[ ,-ina]
        jac.g <- t(as.matrix(jac.g[ ,-ina]))
        vcv <- vcv[-ina,-ina]
    }
    
    VUs <- jac.us%*%vcv%*%t(jac.us)
    lambda <- (VUs[2,2]-VUs[1,2])/(VUs[1,1]+VUs[2,2]-2*VUs[1,2])

    ## the combined U estimate
    Uc <- lambda*U1+(1-lambda)*U2
    sd.U <- sqrt( (VUs[1,1]* VUs[2,2]-VUs[1,2]^2)/(VUs[1,1]+VUs[2,2]-2*VUs[1,2]) )

    ## Calculate the g value similarly
    g <- B.3/B.2

    Vg <- jac.g%*%vcv%*%t(jac.g)
    sd.g <- sqrt(Vg)  
    
    return(list(HLC.Ta=U2, gA=g, sd.gA=sd.g, VarHLC.Ta=VUs[2,2]))
}
