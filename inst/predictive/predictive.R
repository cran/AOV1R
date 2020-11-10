library(AOV1R)

nsims <- 1000
test <- logical(nsims)

for(i in 1:nsims){
  dat <- simAV1R(I=2, J=3, mu=10, sigmab=1, sigmaw=1)
  fit <- aov1r(y ~ group, dat)
  pred <- predict(fit)
  newy <- simAV1R(I=1, J=1, mu=10, sigmab=1, sigmaw=1)$y
  test[i] <- newy > pred[1] && newy < pred[2]
}
mean(test)

# unbalanced
nsims <- 1000
test <- logical(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=3, J=4, mu=10, sigmab=1, sigmaw=1)[-c(1,2),]
  fit <- aov1r(y ~ group, dat)
  pred <- predict(fit)
  newy <- simAV1R(I=1, J=1, mu=10, sigmab=1, sigmaw=1)$y
  test[i] <- newy > pred[1] && newy < pred[2]
}
mean(test)


n <- 20000
Z <- rnorm(n)
I <- 6; J <- 4
U2b <- rchisq(n, I-1)
U2w <- rchisq(n, I*(J-1))

nsims <- 1000
test <- logical(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=I, J=J, mu=10, sigmab=1, sigmaw=1)
  fit <- aov1r(y ~ group, dat)
  pivots <- AOV1R:::pivotal0(fit, Z, U2b, U2w)
  # sims <- numeric(n)
  # for(j in 1:n){
  #   sims[j] <- simAV1R(I=1, J=1,
  #                      mu=pivots$G_mu[j],
  #                      sigmab=sqrt(max(0,pivots$G_sigma2b[j])),
  #                      sigmaw=sqrt(pivots$G_sigma2w[j]))$y
  # }
  sims <- rnorm(n, pivots$G_mu, sqrt(pivots$G_sigma2b+pivots$G_sigma2w))
  pred <- quantile(sims, c(0.025, 0.975))
  newy <- simAV1R(I=1, J=1, mu=10, sigmab=1, sigmaw=1)$y
  test[i] <- newy > pred[1] && newy < pred[2]
}
mean(test) # 0.99 avec la 1ère méthode, pour nsims=100
# 0.985 avec la deuxième pour large nsims (I=2 J=3)

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

