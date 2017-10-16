#' Create a tidy stats data frame from a statistical output object
#'
#' \code{tidy_stats} is a generic function that calls one of the specific \code{tidy_stats} functions dependent on the type of model (e.g., htest, lm, aov).
#'
#' @param model Output of a statistical test
#'
#' @examples
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct statistical tests
#' x <- rnorm(10, mean = 0)
#' y <- rnorm(10, mean = 1)
#'
#' model1 <- t.test(x, y)
#' model2 <- cor.test(x, y)
#' model3 <- lm(y ~ x)
#'
#' # Convert the output to tidy stats data frames
#' tidy_stats(model1)
#' tidy_stats(model2)
#' tidy_stats(model3)
#'
#' @export

tidy_stats <- function(model) UseMethod("tidy_stats")
