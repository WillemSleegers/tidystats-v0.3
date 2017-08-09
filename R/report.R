#' Report function
#'
#' \code{report} is a general function that reports a test in APA style by checking what kind of statistical test is supplied and calling its associated report function.

#' @export
report <- function(results, identifier, term = NULL, statistic = NULL) {

  # Find out which test was used
  method <- results[[identifier]]$method[1]

  # Run the appropriate report function
  if (grepl("t-test", method)) {
    output <- report_t_test(results, identifier, statistic)
  }
  if (grepl("correlation", method)) {
    output <- report_correlation(results, identifier, statistic)
  }
  if (grepl("regression", method)) {
    output <- report_lm(results, identifier, term, statistic)
  }

  return(output)
}
