## B-splines

BSplines <- function(wd)
{
  library(pbs)
  wd
  wd[wd<45] <- wd[wd<45]+360
  #Bs = pbs(wd, df = NULL, knots = c(90,180,270), degree = 2, intercept = T, Boundary.knots = c(0,360))
  Bs <- pbs(wd, df = NULL, knots = c(135, 225,315), degree = 2, intercept = T, Boundary.knots = c(45,405))
}