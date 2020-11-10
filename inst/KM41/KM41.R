KM41 <- data.frame(
  Batch = rep(c("B1","B2","B3","B4","B5"), each=5),
  y = c(379, 357, 390, 376, 376,
        363, 367, 382, 381, 359,
        401, 402, 407, 402, 396,
        402, 387, 392, 395, 394,
        415, 405, 396, 390, 395)
) # saved: data(KM41)

fit <- aov1r(y~Batch, KM41)

I <- fit[["Design"]][["I"]]
J <- fit[["Design"]][["Jh"]]
ssb <- fit[["Sums of squares"]][["ssb"]]
ssw <- fit[["Sums of squares"]][["ssw"]]
n <- 1000000
Z <- rnorm(n)
U2b <- rchisq(n, I-1)
U2w <- rchisq(n, I*(J-1))

X <- Z*sqrt(pmax(0, 1/J*(1+1/I)*ssb/U2b - 1/J*ssw/U2w)) # p 315
quantile(X, .975)


