## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----confidenceIntervals------------------------------------------------------
library(AOV1R)
dat <- simAOV1R(I = 20, J = 5, mu = 10, sigmab = 1, sigmaw = 1)
fit <- aov1r(y ~ group, data = dat)
nsims <- 50000L
gpq <- rGPQ(fit, nsims)
gpq[["GPQ_sigma2tot"]] <- with(gpq, GPQ_sigma2b + GPQ_sigma2w)
# Generalized confidence intervals:
t(vapply(gpq, quantile, numeric(2L), probs = c(2.5, 97.5)/100))

## ----predictiveDistribution---------------------------------------------------
ypred <- with(gpq, rnorm(nsims, GPQ_mu, sqrt(GPQ_sigma2tot)))

## ----predictionInterval-------------------------------------------------------
quantile(ypred, probs = c(2.5, 97.5)/100)

## -----------------------------------------------------------------------------
p <- 90/100
alpha <- 2.5/100
z <- qnorm(p)
GPQ_lowerQuantile <- with(gpq, GPQ_mu - z*sqrt(GPQ_sigma2tot))
GPQ_upperQuantile <- with(gpq, GPQ_mu + z*sqrt(GPQ_sigma2tot))
c(
  quantile(GPQ_lowerQuantile, probs = alpha),
  quantile(GPQ_upperQuantile, probs = 1-alpha)
)

