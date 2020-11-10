library(AOV1R)

dat <- simAV1R(I=3, J=4, mu=0, sigmab=2, sigmaw=3)
dat <- dat[-c(1,2),]

means <- aggregate(y~group, data=dat, FUN=mean)[["y"]]
freqs <- aggregate(y~group, data=dat, FUN=length)[["y"]]

SWb <- function(rho, means, freqs){
  w <- freqs/(1+rho*freqs)
  sum(w*(means-sum(w*means)/sum(w))^2)
}

SWb(0.5, means, freqs)

rho <- seq(0,5,length.out = 50)
swb <- sapply(rho, function(x) SWb(x, means, freqs))
plot(rho, swb, type="l")

nsims <- 2000
sims <- numeric(nsims)
for(i in 1:nsims){
  dat <- simAV1R(I=3, J=4, mu=0, sigmab=2, sigmaw=3)
  dat <- dat[-c(1,2),]
  means <- aggregate(y~group, data=dat, FUN=mean)[["y"]]
  freqs <- aggregate(y~group, data=dat, FUN=length)[["y"]]
  sims[i] <- SWb(4/9, means, freqs)
}

curve(ecdf(sims/9)(x), from=0, to=6)
curve(pchisq(x, 2), add=TRUE, col="red")

