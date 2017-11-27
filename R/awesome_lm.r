#' Awesome Linear Model Fitting
#'
#' \code{awesome_lm} is based on \code{lm}, but also includes standardized coefficients and confidence intervals.
#'
#' @param formula an object of class "formula".
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param weights an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' @param na.action a function which indicates what should happen when the data contain NAs.
#' @param method the method to be used; for fitting, currently only method = "qr" is supported; method = "model.frame" returns the model frame (the same as with model = TRUE, see below).
#' @param model,x,y,qr logicals. If TRUE the corresponding components of the fit (the model frame, the model matrix, the response, the QR decomposition) are returned.
#' @param singular.ok logical. If FALSE (the default in S but not in R) a singular fit is an error.
#' @param contrasts an optional list. See the contrasts.arg of model.matrix.default.
#' @param offset this can be used to specify an a priori known component to be included in the linear predictor during fitting. This should be NULL or a numeric vector of length equal to the number of cases. One or more offset terms can be included in the formula instead or as well, and if more than one are specified their sum is used. See model.offset.
#' @param level the confidence level required.
#' @param ... additional arguments to be passed to the low level regression fitting functions (see below).
#'
#' @details See \code{?lm} for more information. Confidence intervals are created using \code{confint}.
#'
#' @return \code{awesome_lm} returns an object of class "\code{awesome_lm}" and "\code{lm}" or for multiple responses of class c("\code{awesome_mlm}", "\code{mlm}", "\code{awesome_lm}", "\code{lm}").
#'
#' The functions \code{summary} and \code{anova} are used to obtain and print a summary and analysis of variance table of the results.
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

awesome_lm <- function(formula, data, subset, weights, na.action,
                    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
                    singular.ok = TRUE, contrasts = NULL, offset, level = 0.95, ...) {

  # Run a regression
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (method == "model.frame")
    return(mf)
  else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(y))
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                    length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 3) else numeric(), residuals = y,
              fitted.values = 0 * y, weights = w, rank = 0L, df.residual = if (!is.null(w))
                sum(w != 0) else if (is.matrix(y)) nrow(y) else length(y))
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <- if (is.null(w))
      lm.fit(x, y, offset = offset, singular.ok = singular.ok,
             ...)
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                 ...)
  }
  class(z) <- c(if (is.matrix(y)) c("awesome_mlm", "mlm"), c("awesome_lm", "lm"))
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y)
    z$y <- y
  if (!qr)
    z$qr <- NULL

  # Confidence intervals
  z$confidence.intervals <- list(confidence.intervals = confint(z, level = level))

  # Standardized coefficients
  if (length(z$coefficients) > 1) {
    b <- summary(z)$coef[-1, 1]
    sx <- sapply(lapply(z$model[-1], function(x) if (is.factor(x)) as.numeric(x)-1), sd)
    sy <- sapply(z$model[1], sd)
    z$beta <- b * sx /  sy
  }

  return(z)
}
