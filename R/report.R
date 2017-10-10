#' Report function
#'
#' \code{report} is a general function that reports a test in APA style by checking what kind of statistical test is supplied and calling its associated report function.

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
