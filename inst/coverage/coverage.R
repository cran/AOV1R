library(AOV1R)

n <- 15000
Z <- rnorm(n)
I <- 2; J <- 3
U2b <- rchisq(n, I-1)
U2w <- rchisq(n, I*(J-1))

mu <- 10; sigmab <- 1; sigmaw <- 1

nsims <- 1000
test_mu <- test_sigmab <- test_sigmaw <- logical(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=2, J=3, mu=mu, sigmab=sigmab, sigmaw=sigmaw)
  fit <- aov1r(y ~ group, dat)
  pivots <- AOV1R:::pivotal0(fit, Z, U2b, U2w)
  confint_mu <- quantile(pivots[["G_mu"]], c(0.025, 0.975))
  test_mu[i] <- mu > confint_mu[1] && mu < confint_mu[2]
  confint_sigma2b <- quantile(pivots[["G_sigma2b"]], c(0.025, 0.975))
  test_sigmab[i] <- sigmab^2 > confint_sigma2b[1] && sigmab^2 < confint_sigma2b[2]
  confint_sigma2w <- quantile(pivots[["G_sigma2w"]], c(0.025, 0.975))
  test_sigmaw[i] <- sigmaw^2 > confint_sigma2w[1] && sigmaw^2 < confint_sigma2w[2]
}
mean(test_mu)
mean(test_sigmab)
mean(test_sigmaw)

####
set.seed(666)
dat <- simAV1R(I=6, J=2, mu=10, sigmab=2, sigmaw=2)
fit <- aov1r(y~group, dat)
predict(fit)

library(rstanarm)
options(mc.cores = parallel::detectCores())
sfit <- stan_lmer(y ~ (1|group), data=dat,
                  prior_covariance = decov(1, 1, 0.01, 100),
                  iter = 3500, warmup=1000,
                  adapt_delta = 0.98, prior_PD=FALSE)
predictive_interval(sfit, newdata=data.frame(group="xxx"), prob=0.95)
predictive_interval(sfit, newdata=data.frame(group="xxx"), re.form=NA, prob=0.95)

samples <- rstan::extract(sfit$stanfit)
# aux is sigma and theta_L is sigma²_b
psims <- rnorm(10000, samples$alpha, sqrt(samples$theta_L[,1]+samples$aux^2))
quantile(psims, c(0.025, 0.975))

pivotals <- AOV1R:::pivotal(fit)
plot(density(pivotals$G_mu))
lines(density(samples$alpha), col="red")
plot(density(pivotals$G_sigma2b))
lines(density(samples$theta_L[,1]), col="red")
plot(density(pivotals$G_sigma2w))
lines(density(samples$aux^2), col="red")

library(brms)
options(mc.cores = parallel::detectCores())
bfit <- brm(y ~ (1|group), data = dat, control = list(adapt_delta = 0.95),
            prior = c(prior(cauchy(0,5),class="sd")),
            iter = 3500, warmup = 1000)
pred <- posterior_predict(bfit, newdata=data.frame(group="xxx"), allow_new_levels=TRUE)
quantile(pred, c(0.025, 0.975))

samples <- posterior_samples(bfit)
names(samples)
psims <- rnorm(10000, samples$b_Intercept,
               sqrt(samples$sd_group__Intercept^2 + samples$sigma^2))
quantile(psims, c(0.025, 0.975))

pivotals <- AOV1R:::pivotal(fit)
plot(density(pivotals$G_mu))
lines(density(samples$b_Intercept), col="red")
plot(density(pivotals$G_sigma2b))
lines(density(samples$sd_group__Intercept^2), col="red")
plot(density(pivotals$G_sigma2w))
lines(density(samples$sigma^2), col="red")
plot(density(pivotals$G_sigma2b+pivotals$G_sigma2w, from=0, to=200))
lines(density(samples$sd_group__Intercept^2+samples$sigma^2), col="red")

# assez nickel !

plot(pivotals$G_mu[1:2000], pivotals$G_sigma2w[1:2000])
points(samples$b_Intercept, samples$sigma^2, col="red")

library(lme4)
lfit <- lmer(y ~ (1|group), dat)

