library(AOV1R)

set.seed(666)
I=3; J=4
dat <- simAV1R(I, J, mu=0, sigmab=2, sigmaw=3)
fit <- aov1r(y ~ group, data=dat)
ssb <- fit[["Sums of squares"]][["ssb"]]
ssw <- fit[["Sums of squares"]][["ssw"]]
total_variance <- sum(fit[["Variance components"]])

# Satterwaithe degrees of freedom of the total variance
a <- 1/J/(I-1)
b <- (1-1/J) * 1/I/(J-1)
(a*ssb+b*ssw)^2/((a*ssb)^2/(I-1) + (b*ssw)^2/(I*(J-1)))

# other way to get the Satterwaithe df
library(VCA)
vca <- anovaMM(y ~ (group), Data=dat)
# estimated variance of total variance
var_total_var <- sum(vcovVC(vca))
2*total_variance^2 / var_total_var # = Satterwaithe df

