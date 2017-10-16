#' Add statistical output to a tidy stats list
#'
#' \code{add_stats} adds output to a results list. It can take either the output of a statistical test as input or a data frame. See Details for more information on adding data frames.
#'
#' @param output output of a statistical test or a data frame. If a data frame is provided, it must already be in a tidy format.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param statistics a vector of statistics to select from the output and add to the tidy stats list.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", "descriptives", or "other". Can be abbreviated.
#' @param confirmatory a boolean to indicate whether the statistical test was confirmatory (TRUE) or exploratory (FALSE). Can be NA.
#' @param notes a character string to add additional information. Some statistical tests produce notes information, which will be overwritten if notes are provided.
#'
#' @details Some statistical functions produce unidentifiable output, which means \code{tidystats} cannot figure out how to tidy the data. To add these results, the output should be manually tidied or tidied using one of \code{tidystats}'s tidy functions. See \code{?tidy_stats} for an overview of available functions.
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
#' # Add output to the results list
#' results <- add_stats(model1, results, identifier = "M1",
#'                      statistics = c("t", "df", "p"), type = "h",
#'                      confirmatory = TRUE, notes = "An example t-test")
#' results <- add_stats(model2, results, identifier = "M2")
#' results <- add_stats(model3, results, identifier = "M3")
#'
#' # Get confidence intervals of model3
#' ci <- confint(model3)
#'
#' # Produce a tidy data frame of the CIs and add it to the results list
#' results <- ci %>%
#'   tidy_stats_confint() %>%
#'   add_stats(results, identifier = "M3_CIs")
#'
#' @export

add_stats <- function(output, results, identifier = NULL, statistics = NULL, type = NULL,
                      confirmatory = NULL, notes = NULL) UseMethod("add_stats")
