library(AOV1R)

I <- 2; J <- 3
dat <- simAV1R(I=I, J=J, mu=10, sigmab=1, sigmaw=1)
fit <- aov1r(y ~ group, dat)
pivots <- pivotal(fit)
predict(fit)
quantile(pivots$G_mu, c(0.025, 0.975))


I <- 2; J <- 3
dat <- simAV1R(I=I, J=J, mu=10, sigmab=1, sigmaw=1)
fit <- aov1r(y ~ group, dat)
pivots <- pivotal(fit)
ybar <- fit$grandmean

sigma2 <- median((pivots$G_sigma2b + pivots$G_sigma2w) +
  (J*pivots$G_sigma2b + pivots$G_sigma2w)/I/J)

ybar + c(-1,1)*qnorm(.975, mean=0, sd=sqrt(sigma2))
# me semble trop petit
predict(fit)
ybar + c(-1,1)*median(qnorm(.975, mean=0, sd = sqrt((pivots$G_sigma2b + pivots$G_sigma2w) +
                                       (J*pivots$G_sigma2b + pivots$G_sigma2w)/I/J)))

predict(fit)


