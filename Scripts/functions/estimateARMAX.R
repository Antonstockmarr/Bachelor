estimateARMAX <- function(outName, inNames, pAR, pMA=1, noLagPattern=NA, pNoLag=1, printit=TRUE)
{
    ## Testing parameters
    ## outName <- 'Qi'
    ## inNames <- c('Ti','Te')
    ## pAR=1
    ## noLagPattern = "Te"
    ## pNoLag <- 1
    ## Generate a formula and fit it for an ARMAX model of model order pAR and pMA using the package 'marima'

    ## Take out the data needed for fitting the model
    Dat <- X[ ,c(outName,inNames)]
    ## Add an additional column, this must be a DUMMY variable put in due to need for extracting lm!??
    set.seed(1287)
    Dat$NAout <- Dat$Qi + rnorm(nrow(Dat))

    ## Generate the model pattern
    Mod <- define.model(kvar=ncol(Dat), ar=1:pAR, ma=1:pMA, reg.var=2:(ncol(Dat)-1), indep=ncol(Dat), no.dep=c(1,ncol(Dat)))

    arp <- Mod$ar.pattern
    map <- Mod$ma.pattern
    ## ## Print out model in 'short form':
    ## Set the inputs matching the noLagPattern
    inolag <- grep(noLagPattern, names(Dat)[-ncol(Dat)])
    if(length(inolag) > 0){
        if(pNoLag < pAR){
            arp[1, inolag, (pNoLag+2):dim(arp)[3]] <- 0
        }
    }
    if(printit){
        print("ar.pattern")
        print(short.form(arp))
    }
##    short.form(map)

    ## A hack to get out the parameters in the right way
    ## Lav environment på marima.pbac funktionen til marima
    environment(marima.pbac) <- asNamespace('marima')
    ##
    fitlm <- marima.pbac(t(Dat), ar.pattern=arp, ma.pattern=map, penalty=0.0, Plot='log.det', means=0)
    ## Change the names of the coefficients
    names(fitlm$coefficients) 

    nms <- names(Dat)[-ncol(Dat)]
    coefnms <- paste0(nms,".l1")
    if(pAR > 1){
        for(i in 2:pAR){
            if(i > pNoLag){
                coefnms <- c(coefnms, paste0(nms[-inolag],".l",i))
            }else{
                coefnms <- c(coefnms, paste0(nms,".l",i))
            }
        }
    }
    coefnms <- c(coefnms, paste0(rep('ma.l',pMA),1:pMA))
    
## OLD        c(paste0(rep(c(outName,inNames), pAR),'.l',rep(1:pAR,each=length(outName)+length(inNames))), paste0(rep('ma',pMA),1:pMA))

    names(fitlm$coefficients) <- coefnms
    ## Return the fit
    return(fitlm)
}
