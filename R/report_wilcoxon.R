#' Report function for Wilcoxon Rank Sum and Signed Rank Tests
#'
#' Function to report Wilcoxon Rank Sum and Signed Rank Tests in APA style.
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
#' report(identifier = "wilcoxon_rank_sum")
#' report(identifier = "wilcoxon_signed_rank")
#'
#' @export

report_wilcoxon <- function(identifier, results = getOption("tidystats_list")) {

  output <- NULL

  # Extract model results
  res <- results[[identifier]]

  # Check if all the necessary statistics are there to produce a line of output
  if (sum(c("V", "W", "p") %in% unique(res$statistic)) == 2) {
    # Extract statistics
    if ("V" %in% res$statistic) {
      test_statistic <- "V"
    } else {
      test_statistic <- "W"
    }

    test_value <- dplyr::pull(dplyr::filter(res, statistic == test_statistic),
      value)
    p <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

    test_value <- report_statistic(test_statistic, test_value)
    p <- report_p_value(p)

    output <- paste0("*", test_statistic, "* = ", test_value, ", ", p)

    # Add the confidence interval, if it exists
    res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[0-9]+% CI"))

    if (nrow(res_CI) > 0) {
      CI_pct <- readr::parse_number(dplyr::first(dplyr::pull(res_CI,
        statistic)))

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
