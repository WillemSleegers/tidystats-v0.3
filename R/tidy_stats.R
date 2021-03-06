#' Create a tidy stats data frame from a statistical output object
#'
#' \code{tidy_stats} is a generic function that calls one of the specific
#' \code{tidy_stats} functions dependent on the type of model
#' (e.g., htest, lm, aov).
#'
#' @param model Output of a statistical test
#' @param args Unused.
#'
#' @examples
#'
#' # Example: t-test
#' model_t_test <- t.test(extra ~ group, data = sleep)
#' tidy_stats(model_t_test)
#'
#' # Example: correlation
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#' model_correlation <- cor.test(x, y)
#' tidy_stats(model_correlation)
#'
#' # Example: Regression
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model_lm <- lm(weight ~ group)
#' tidy_stats(model_lm)
#'
#' # Example: ANOVA
#' model_aov <- aov(yield ~ block + N * P * K, npk)
#' tidy_stats(model_aov)
#'
#' # Example: Within-subjects ANOVA
#' model_aov_within <- aov(extra ~ group + Error(ID/group), data = sleep)
#' tidy_stats(model_aov_within)
#'
#' @export

tidy_stats <- function(model, args = NULL) UseMethod("tidy_stats")
