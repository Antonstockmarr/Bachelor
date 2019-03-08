newopti <- function(a)
{
  fit <- lm(tempq ~ 1 + ((temp<a)*(temp-a)))
  res <- sum(fit$residuals)
  res
}
