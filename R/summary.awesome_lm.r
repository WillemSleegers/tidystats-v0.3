#' Summarizing Awesome Linear Model Fits
#'
#' \code{summary} method for class "\code{awesome_lm}"
#'
#' @param object an object of class "lm", usually, a result of a call to lm.
#' @param x an object of class "summary.lm", usually, a result of a call to summary.lm.
#' @param correlation logical; if TRUE, the correlation matrix of the estimated parameters is returned and printed.
#' @param digits the number of significant digits to use when printing.
#' @param symbolic.cor logical. If TRUE, print the correlations in a symbolic form (see symnum) rather than as numbers.
#' @param signif.stars logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @param ...	 further arguments passed to or from other methods.
#'
#' @details code{summary.awesome_lm} is based on \code{summary}.
#'
#' @seealso \code{awesome_lm}.
#'
#' @examples
#'
#' # Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' # Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' # Example 1: Standard regresssion
#' lm1 <- lm(weight ~ group)
#' summary(lm1)
#'
#' # Example 2: 90% confidence intervals
#' lm2 <- lm(weight ~ group, level = .9)
#' summary(lm2)
#'
#' @export

summary.awesome_lm<- function(object, correlation = FALSE, symbolic.cor = FALSE, ...) {

  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    class(ans) <- "summary.lm"
    ans$aliased <- is.na(coef(object))
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("b", "SE", "beta", "t", "p"))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    return(ans)
  }
  if (is.null(z$terms))
    stop("invalid 'lm' object:  no 'terms' component")
  if (!inherits(object, "lm"))
    warning("calling summary.lm(<fake-lm-object>) ...")
  Qr <- stats:::qr.lm(object)
  n <- NROW(Qr$qr)
  if (is.na(z$df.residual) || n - p != z$df.residual)
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept"))
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) *
      1e-30)
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  betas <- c(NA, z$beta)
  tval <- est/se
  ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
  ans$residuals <- r
  ans$coefficients <- cbind(b = est, SE = se, beta = betas, t = tval,
                            p = 2 * pt(abs(tval), rdf,lower.tail = FALSE),
                            z$confidence.intervals[[1]])
  ans$aliased <- is.na(z$coefficients)
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, NCOL(Qr$qr))
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept"))
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n -
                                                       df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
                        numdf = p - df.int, dendf = rdf)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,
                                                             1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if (!is.null(z$na.action))
    ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}

