estimateARMAX <- function(outName, inNames, pAR, pMA=1)
{
  ## Testing parameters
  ## outName <- 'Qi'
  ## inNames <- c('Ti','Te')
  ## pAR=1
  ## Generate a formula and fit it for an ARMAX model of model order pAR and pMA using the package 'marima'

  ## Take out the data needed for fitting the model
  Dat <- X[ ,c(outName,inNames)]
  ## Add an additional column
  set.seed(1287)
  Dat$NAout <- Dat$Qi + rnorm(nrow(Dat))

  ## Generate the model pattern
  Mod <- define.model(kvar=ncol(Dat), ar=1:pAR, ma=1:pMA, reg.var=2:(ncol(Dat)-1), indep=ncol(Dat), no.dep=c(1,ncol(Dat)))
  arp <- Mod$ar.pattern
  map <- Mod$ma.pattern
  ## ## Print out model in 'short form':
  ## short.form(arp)
  ## short.form(map)

  ## A hack to get out the parameters in the right way
  ## Lav environment på marima.pbac funktionen til marima
  environment(marima.pbac) <- asNamespace('marima')
  ##
  fitlm <- marima.pbac(t(Dat), ar.pattern=arp, ma.pattern=map, penalty=0.0, Plot='log.det', means=0)

  ## print(marima(t(Dat), ar.pattern=arp, ma.pattern=map, penalty=0.0, Plot='log.det', means=0))
  ## summary(fitlm)

  ## Change the names of the coefficients
  names(fitlm$coefficients) <- c(paste0(rep(c(outName,inNames), pAR),'.l',rep(1:pAR,each=length(outName)+length(inNames))), paste0(rep('ma',pMA),1:pMA))

  ## Return the fit
  return(fitlm)
}
