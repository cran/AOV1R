#' @name aov1r
#' @rdname aov1r
#' @title One-way random effect ANOVA
#' @description Fits a one-way random effect ANOVA model.
#'
#' @param formula a formula of the form \code{y~group}
#' @param data optional dataframe
#' @param x output of \code{summary}
#' @param object an \code{aov1r} object (output of an \code{aov1r} call)
#' @param ... ignored
#'
#' @return \code{aov1r} returns an object of class \code{aov1r};
#' @import data.table
#' @importFrom lazyeval f_eval_lhs f_eval_rhs f_lhs f_rhs
#'
#' @examples
#' dat <- simAOV1R(I=2, J=3, mu=10, sigmab=1, sigmaw=1)
#' fit <- aov1r(y ~ group, data=dat)
#' summary(fit)
NULL

#' @rdname aov1r
#' @export
aov1r <- function(formula, data=NULL){
  DT <- data.table(y = lazyeval::f_eval_lhs(formula, data=data),
                   group = lazyeval::f_eval_rhs(formula, data=data))
  DT[, means := mean(y), by="group"]
  ssw <- with(DT, crossprod(y-means)[1L,1L])
  DT2 <- DT[, list(means = means[1L], Ji = .N), by="group"]
  DT2[, Mean:=mean(means)]
  balanced <- all(DT2[["Ji"]][1L] == DT2[["Ji"]][-1L])
  I <- nrow(DT2)
  Jh <- I/sum(1/DT2[["Ji"]])
  ssb <- Jh*with(DT2, crossprod(Mean-means)[1L,1L])
  terms <- c(y = as.character(lazyeval::f_lhs(formula)),
             group = as.character(lazyeval::f_rhs(formula)))
  N <- nrow(DT)
  out <- list(
    "Sums of squares" = c(ssw=ssw, ssb=ssb),
    "Variance components" = c(sigma2w = ssw/(N-I), sigma2b = (ssb/(I-1)-ssw/(N-I))/Jh),
    "Design" = c(I=I, Jh=Jh, N=N),
    "Balanced" = balanced,
    "grandMean" = DT2$Mean[1L],
    "groupMeans" =
      setNames(
        as.data.frame(DT2[, .SD, .SDcols=c("group", "means")]),
        c("group", "mean")),
    "data" = data[, terms],
    "terms" = terms
  )
  class(out) <- "aov1r"
  out
}

#' @rdname aov1r
#' @export
summary.aov1r <- function(object, ...){
  out <- list()
  class(out) <- "summary.aov1r"
  out[["Response"]] <- object$terms[["y"]]
  out[["Factor"]] <- object$terms[["group"]]
  attr(out, "Balanced") <- object[["Balanced"]]
  out
}

#' @rdname aov1r
#' @export
print.summary.aov1r <- function(x, ...){
  for(foo in names(x)){
    cat(foo, ": ", x[[foo]], "\n", sep="")
  }
  if(attr(x, "Balanced")){
    cat("Design is balanced.\n")
  }else{
    cat("Design is *not* balanced.\n")
  }
}

#' @title Prediction interval for one-way random effect ANOVA
#' @description Prediction interval for the one-way random effect ANOVA model,
#'   based on a Satterthwaite approximation of the degrees of freedom.
#'
#' @param object an output of \code{\link{aov1r}}
#' @param level confidence level
#' @param ... ignored
#'
#' @return A vector of length two, the bounds of the prediction interval.
#' @export
#' @importFrom stats qt
#'
#' @references T. Y. Lin, C. T. Liao.
#'   \emph{Prediction intervals for general balanced linear random models}.
#'   Journal of Statistical Planning and Inference 138 (2008), 3164 – 3175.
#'   <doi:10.1016/j.jspi.2008.01.001>
#'
#' @examples
#' dat <- simAOV1R(I=2, J=3, mu=10, sigmab=1, sigmaw=1)
#' fit <- aov1r(y ~ group, data=dat)
#' predict(fit)
predict.aov1r <- function(object, level=0.95, ...){
  I <- object[["Design"]][["I"]]
  J <- object[["Design"]][["Jh"]]
  N <- object[["Design"]][["N"]]
  SSb <- object[["Sums of squares"]][["ssb"]]
  SSw <- object[["Sums of squares"]][["ssw"]]
  a <- (1/J*(1+1/I))/(I-1)
  b <- (J-1)/J/(N-I)
  v <- a*SSb+b*SSw # estimates the variance of (Ynew-Ybar)
  nu <- v^2/((a*SSb)^2/(I-1)+(b*SSw)^2/(N-I)) # Satterthwaite degrees of freedom
  alpha.over.two <- (1-level)/2
  bounds <-  object[["grandMean"]] +
    c(-1,1)*sqrt(v)*qt(1-alpha.over.two, nu)
  names(bounds) <- paste0(100*c(alpha.over.two, 1-alpha.over.two), "%")
  attr(bounds, "std.error") <- sqrt(v)
  attr(bounds, "df") <- nu
  bounds
}


#' @title Confidence intervals
#' @description Confidence intervals for the one-way random effect ANOVA.
#'
#' @param object an output of \code{\link{aov1r}}
#' @param parm ignored
#' @param level confidence level
#' @param SDs logical, whether to return confidence intervals about the
#'   standard deviations or about the variances
#' @param x an output of \code{confint} applied to an \code{aov1r} object
#' @param ... ignored
#'
#' @return A dataframe providing the bounds of the confidence
#'   intervals.
#'
#' @references Richard K. Burdick, Franklin. A. Graybill.
#' \emph{Confidence Intervals on Variance Components}.
#' CRC Press; 1st edition (1992).
#' ISBN-13: 978-0824786441.
#'
#' @export
#' @importFrom stats qf qt sd
#'
#' @examples
#' dat <- simAOV1R(I=2, J=3, mu=10, sigmab=1, sigmaw=1)
#' fit <- aov1r(y ~ group, data=dat)
#' confint(fit)
confint.aov1r <- function(object, parm, level = 0.95, SDs = TRUE, ...){
  I <- object[["Design"]][["I"]]
  J <- object[["Design"]][["Jh"]]
  balanced <- object[["Balanced"]]
  if(!balanced){
    warning(
      "Design is not balanced - confidence intervals are not valid."
    )
  }
  SSb <- object[["Sums of squares"]][["ssb"]]
  SSw <- object[["Sums of squares"]][["ssw"]]
  sigma2w <- object[["Variance components"]][["sigma2w"]]
  sigma2b <- object[["Variance components"]][["sigma2b"]]
  DFb <- I - 1 # between df
  DFw <- I * (J - 1) # within df
  MSSb <- SSb/DFb; MSSw <- SSw/DFw # mean sums of squares
  a <- (1 - level) / 2
  ## grandMean confidence interval
  tstar <- qt(1-a, DFb)
  stdev <- sd(object[["groupMeans"]][["mean"]])
  muLCB <- object[["grandMean"]] - tstar * stdev / sqrt(I)
  muUCB <- object[["grandMean"]] + tstar * stdev / sqrt(I)
  ## Within variance confidence interval
  withinLCB <- sigma2w / qf(1-a, DFw, Inf)  # Within lwr
  withinUCB <- sigma2w / qf(a, DFw, Inf) # Within upr
  ## Between variance confidence interval
  G1 <- 1 - (1 / qf(1-a, DFb, Inf))
  G2 <- 1 - (1 / qf(1-a, DFw, Inf))
  H1 <- (1 / qf(a, DFb, Inf)) - 1
  H2 <- (1 / qf(a, DFw, Inf)) - 1
  G12 <- ((qf(1-a, DFb, DFw) - 1)^2 - (G1^2 * qf(1-a, DFb, DFw)^2) - (H2^2)) /
    qf(1-a, DFb, DFw)
  H12 <- ((1 - qf(a, DFb, DFw))^2 - H1^2 * qf(a, DFb, DFw)^2 - G2^2) /
    qf(a, DFb, DFw)
  Vu <- H1^2 * MSSb^2 + G2^2 * MSSw^2 + H12 * MSSb * MSSw
  Vl <- G1^2 * MSSb^2 + H2^2 * MSSw^2 + G12 * MSSw * MSSb
  betweenLCB <- (MSSb - MSSw - sqrt(Vl)) / J # Betwen lwr
  betweenUCB <- (MSSb - MSSw + sqrt(Vu)) / J # Between upr
  ## Total variance confidence interval
  sigma2tot <- sigma2w + sigma2b # estimate
  totalLCB <- sigma2tot - (sqrt(G1^2 * MSSb^2 + G2^2 * (J - 1)^2 * MSSw^2) / J) # Total lwr
  totalUCB <- sigma2tot + (sqrt(H1^2 * MSSb^2 + H2^2 * (J - 1)^2 * MSSw^2) / J) # Total upr
  # Output
  estimate <- c(sigma2w, sigma2b, sigma2tot)
  lwr <- c(withinLCB, betweenLCB, totalLCB)
  upr <- c(withinUCB, betweenUCB, totalUCB)
  if(SDs){
    estimate <- sign(estimate) * sqrt(abs(estimate))
    lwr <- sign(lwr) * sqrt(abs(lwr))
    upr <- sign(upr) * sqrt(abs(upr))
  }
  out <- data.frame(
    estimate = c(object[["grandMean"]], estimate),
    lwr = c(muLCB, lwr),
    upr = c(muUCB, upr)
  )
  rownames(out) <- c("grandMean", "within", "between", "total")
  attr(out, "confidence level") <- level
  attr(out, "standard deviations") <- SDs
  class(out) <- c("confint.aov1r", class(out))
  out
}

#' @rdname confint.aov1r
#' @importFrom utils capture.output
#' @export
print.confint.aov1r <- function(x, ...){
  cat(capture.output(print.data.frame(x)), sep = "\n")
  cat('\nattr(,"confidence level")\n')
  cat(capture.output(attr(x,"confidence level")))
  cat('\nattr(,"standard deviations")\n')
  cat(capture.output(attr(x,"standard deviations")), "\n")
}
