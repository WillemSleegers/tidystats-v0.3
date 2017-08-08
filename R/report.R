#' Report function
#'
#' \code{report} is a general function that reports a test in APA style by checking what kind of statistical test is supplied and calling its associated report function.

#' @export
report <- function(results, identifier, term = NULL, statistic = NULL) {
  # Find out which test was used
  method <- results[[identifier]]$method[1]

  # Run the appropriate report function
  output <- case_when(
    grepl("t-test", method) ~ report_t_test(results, identifier, statistic)
  )

  return(output)
}
