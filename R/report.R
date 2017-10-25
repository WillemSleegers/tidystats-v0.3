#' Report function
#'
#' \code{report} is a general function that returns Markdown code of a statistical test in 6th edition APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @details \code{report} calls a specific report function dependent on the type of statistical test that is supplied. The 'method' column of the statistical test is used to determine which report function to run.
#'
#' @examples
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct statistical tests
#' model <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' # Add output to the results list
#' results <- add_stats(model, results, identifier = "M1")
#'
#' # Report results
#' report(results, identifier = "M1")
#'
#' @export

report <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Find out which test was used
  method <- results[[identifier]]$method[1]

  # Run the appropriate report function
  if (grepl("t-test", method)) {
    output <- report_t_test(results, identifier, statistic)
  } else {
    if (grepl("correlation", method)) {
      output <- report_correlation(results, identifier, statistic)
    } else {
      if (grepl("regression", method)) {
        output <- report_lm(results, identifier, term, term_nr, statistic)
      } else {
        if (grepl("ANOVA|ANCOVA", method)) {
          output <- report_anova(results, identifier, term, term_nr, statistic)
        }
      }
    }
  }

  return(output)
}
