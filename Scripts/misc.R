varX <- 10
varY <- 1
corXY <- 0.9

(covXY <- corXY * sqrt(varX) * sqrt(varY))

lambda <- 1:40/10-2
plot(lambda, lambda^2*varX + (1-lambda)^2*varY + 2*lambda*(1-lambda)*covXY)
## 
