library(AOV1R)

set.seed(666)
I=3; J=4
dat <- simAV1R(I, J, mu=0, sigmab=2, sigmaw=3)
fit <- aov1r(y ~ group, data=dat)
ssb <- fit[["Sums of squares"]][["ssb"]]
ssw <- fit[["Sums of squares"]][["ssw"]]
sigma2b <- fit[["Variance components"]][["sigma2b"]]
sigma2w <- fit[["Variance components"]][["sigma2w"]]
total_variance <- sum(fit[["Variance components"]])

# standard error of the fixed effect
( stderr <- sqrt(ssb/(I-1)/I/J) )
lfit <- lme4::lmer(y ~ (1|group), data=dat)
summary(lfit)$coefficients

I=3; J=4
nsims <- 3000
sims_stderr <- numeric(nsims)
sims_totalVariance <- numeric(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I, J, mu=0, sigmab=2, sigmaw=3)
  fit <- aov1r(y ~ group, data=dat)
  ssb <- fit[["Sums of squares"]][["ssb"]]
  sims_stderr[i] <- sqrt(ssb/(I-1)/I/J)
  sims_totalVariance[i] <- sum(fit[["Variance components"]])
}
mean(sims_stderr^2) # a/b if Gamma(a,b)
var(sims_stderr^2) # a/b^2
( b <- mean(sims_stderr^2) / var(sims_stderr^2) )
( a <- b * mean(sims_stderr^2) ) # (I-1)/2

plot(sims_stderr, sims_totalVariance,
     xlab="standard error of intercept",
     ylab="estimate of total variance")

plot(sims_stderr^2, sims_totalVariance,
     xlab="squared standard error of intercept (constant times SSb)",
     ylab="estimate of total variance")


library(lmerTest)
lfit <- lmerTest::lmer(y ~ (1|group), data=dat)

library(nlme)
lmefit <- lme(y ~ 1, random = list(group = ~ 1), data=dat)

