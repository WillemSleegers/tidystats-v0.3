#' Report function for Wilcoxon Rank Sum and Signed Rank Tests
#'
#' Function to report Wilcoxon Rank Sum and Signed Rank Tests in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param statistic A character string identifying the exact statistic you want
#' to report.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "wilcoxon_signed_rank")
#' report(results, identifier = "wilcoxon_signed_rank", statistic = "Z")
#'
#' @export

report_wilcoxon <- function(results, identifier, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Check whether a single statistic is requested, or a full line of APA output
  if (!is.null(statistic)) {
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    } else {
      res_statistic = statistic
    }

    value <- dplyr::pull(dplyr::filter(res, statistic == res_statistic), value)
    output <- report_statistic(res_statistic, value)
  } else {

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

    output <- paste0(test_statistic, " = ", test_value, ", ", p)

    # Add confidence interval, if it exists
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
