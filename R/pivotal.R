#' Generalized pivotal quantities
#'
#' Simulates from the generalized pivotal quantities.
#'
#' @param fit an \code{\link{aov1r}} object
#' @param n number of simulations
#'
#' @return The simulations in a dataframe.
#'
#' @references Samaradasa Weerahandi.
#' \emph{Exact Statistical Methods for Data Analysis}.
#' Springer, New York, NY (1995).
#' <doi:10.1007/978-1-4612-0825-9>
#'
#' @importFrom stats rchisq
#' @export
#'
#' @examples
#' dat <- simAOV1R(I=20, J=5, mu=10, sigmab=1, sigmaw=1)
#' fit <- aov1r(y ~ group, data=dat)
#' nsims <- 20000
#' pivsims <- rGPQ(fit, nsims)
#' pivsims$GPQ_sigma2tot <- pivsims$GPQ_sigma2b + pivsims$GPQ_sigma2w
#' # Generalized confidence intervals:
#' lapply(pivsims, quantile, probs = c(0.025, 0.975))
#' # compare with the frequentist confidence intervals:
#' confint(fit, SDs = FALSE)
#' # Generalized prediction interval:
#' with(
#'   pivsims,
#'   quantile(rnorm(nsims, GPQ_mu, sqrt(GPQ_sigma2tot)),
#'            probs = c(0.025, 0.975))
#' )
#' # compare with the frequentist prediction interval:
#' predict(fit)
rGPQ <- function(fit, n=10000){
  I <- fit[["Design"]][["I"]]
  J <- fit[["Design"]][["Jh"]]
  N <- fit[["Design"]][["N"]]
  ssb <- fit[["Sums of squares"]][["ssb"]]
  ssw <- fit[["Sums of squares"]][["ssw"]]
  Z <- rnorm(n)
  U2b <- rchisq(n, I-1)
  U2w <- rchisq(n, N-I)
  data.frame(
    GPQ_mu = fit[["grandMean"]] - Z/sqrt(U2b)*sqrt(ssb/I/J),
    GPQ_sigma2b = 1/J*(ssb/U2b - ssw/U2w),
    GPQ_sigma2w = ssw/U2w
  )
}

# pivotal0 <- function(fit, Z, U2b, U2w){
#   I <- fit[["Design"]][["I"]]
#   J <- fit[["Design"]][["Jh"]]
#   ssb <- fit[["Sums of squares"]][["ssb"]]
#   ssw <- fit[["Sums of squares"]][["ssw"]]
#   list(
#     G_mu = fit[["grandmean"]] - Z/sqrt(U2b)*sqrt(ssb/I/J),
#     G_sigma2b = 1/J*(ssb/U2b - ssw/U2w),
#     G_sigma2w = ssw/U2w
#   )
# }
