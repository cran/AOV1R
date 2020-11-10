library(AOV1R)

dat <- simAV1R(I = 5, J = 3, mu = 10, sigmab = 1, sigmaw = 1)
fit <- aov1r(y ~ group, dat)
pivots <- pivotal(fit)
sims_qupp <- qnorm(0.975, pivots$G_mu, sqrt(pivots$G_sigma2b+pivots$G_sigma2w))
sims_qlow <- qnorm(0.025, pivots$G_mu, sqrt(pivots$G_sigma2b+pivots$G_sigma2w))
median(sims_qlow); median(sims_qupp)
predict(fit)
