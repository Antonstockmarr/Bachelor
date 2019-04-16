## B-splines

BSplines <- function(wd)
{
  library(pbs)
  wd = wd[order(wd)]
  lasse = pbs(wd, df = NULL, knots = c(90,180,270), degree = 2, intercept = T, Boundary.knots = c(0,360))
}