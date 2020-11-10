library(AOV1R)

# check law SSb (error on my blog!)
I <- 2; J <- 5
mu <- 10; sigmab <- 2; sigmaw <- 3
sigma2 <- J*sigmab^2 + sigmaw^2
nsims <- 1000
result <- numeric(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=I, J=J, mu=mu, sigmab=sigmab, sigmaw=sigmaw)
  fit <- aov1r(y ~ group, dat)
  result[i] <- fit$`Sums of squares`[["ssb"]]
}
ssbs <- result/sigma2/(I-1)
curve(ecdf(ssbs)(x), from=0, to=2)
curve(pchisq(x, I-1), add=TRUE, col="red")

# check estimates
I <- 2; J <- 3
mu <- 10; sigmab <- 2; sigmaw <- 3
nsims <- 50000
sigma2b <- sigma2w <- numeric(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=I, J=J, mu=mu, sigmab=sigmab, sigmaw=sigmaw)
  fit <- aov1r(y ~ group, dat)
  estimates <- fit$`Variance components`
  sigma2w[i] <- estimates[["sigma2w"]]
  sigma2b[i] <- estimates[["sigma2b"]]
}
mean(sigma2w)
mean(sigma2b)
