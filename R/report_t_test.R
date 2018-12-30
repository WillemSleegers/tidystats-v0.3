#' Report function for t-tests
#'
#' Function to report a t-test in APA style.
#'
#' @param identifier A character string identifying the model.
#' @param results A tidystats list.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the default tidystats list in options()
#' options(tidystats_list = results)
#'
#' # Report results
#' report(identifier = "t_test_one_sample")
#' report(identifier = "t_test_two_sample")
#' report(identifier = "t_test_welch")
#' report(identifier = "t_test_paired")
#'
#' @export
report_t_test <- function(identifier, results = getOption("tidystats_list")) {

  output <- NULL

  # Extract model results
  res <- results[[identifier]]

  # Check if all the necessary statistics are there to produce a line of output
  if (sum(c("t", "df", "p") %in% unique(res$statistic)) == 3) {
    # Extract statistics
    t  <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
    df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
    p  <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

    t  <- report_statistic("t", t)
    df <- report_statistic("df", df)
    p  <- report_p_value(p)

    output <- paste0("*t*(", df, ") = ", t, ", ", p)

    # Add a confidence interval, if it exists
    res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[0-9]+% CI"))

    if (nrow(res_CI) > 0) {
      CI_pct <- readr::parse_number(first(pull(res_CI, statistic)))

      CI_lower <- dplyr::pull(res_CI, value)[1]
      CI_upper <- dplyr::pull(res_CI, value)[2]

      CI_lower <- report_statistic("CI", CI_lower)
      CI_upper <- report_statistic("CI", CI_upper)

      CI <- paste0(CI_pct, "% CI ", "[", CI_lower, ", ", CI_upper, "]")

      output <- paste0(output, ", ", CI)
    }
  }

  return(output)
}
