#' Report function for t-tests
#'
#' Function to report a t-test in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "t_test_one_sample")
#' report(results, identifier = "t_test_two_sample")
#' report(results, identifier = "t_test_welch")
#' report(results, identifier = "t_test_paired")
#'
#' @export

#TODO: Add check to all report functions to see whether res contains all required statistics

report_t_test <- function(results, identifier) {

  # Extract model results
  res <- results[[identifier]]

  # Extract statistics
  t  <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
  df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
  p  <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

  t  <- report_statistic("t", t)
  df <- report_statistic("df", df)
  p  <- report_p_value(p)

  output <- paste0("*t*(", df, ") = ", t, ", ", p)

  # Add a confidence interval, if it exists
  res_CI <- filter(res, str_detect(statistic, "[0-9]+% CI"))

  if (nrow(res_CI) > 0) {
    CI_pct <- parse_number(first(pull(res_CI, statistic)))

    CI_lower <- pull(res_CI, value)[1]
    CI_upper <- pull(res_CI, value)[2]

    CI_lower <- report_statistic("CI", CI_lower)
    CI_upper <- report_statistic("CI", CI_upper)

    CI <- paste0(CI_pct, "% CI ", "[", CI_lower, ", ", CI_upper, "]")

    output <- paste0(output, ", ", CI)
  }

  return(output)
}
