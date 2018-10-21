#' Report function for Fisher's Exact Tests for Count Data
#'
#' Function to report Fisher's Exact Tests for Count Data in APA style.
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
#' report(results, identifier = "fisher_test")
#' report(results, identifier = "fisher_test", statistic = "OR")
#'
#' @export

report_fisher <- function(results, identifier, statistic = NULL) {

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

    p <- dplyr::filter(res, statistic == "p") %>%
      dplyr::pull(value)
    p <- report_p_value(p)

    if ("OR" %in% res$statistic) {
      OR <- dplyr::filter(res, statistic == "OR") %>%
        dplyr::pull(value)

      OR <- report_statistic("OR", OR)

      output <- paste0("OR = ", OR, ", ", p)
    } else {
      output <- p
    }

    # Add confidence interval, if it exists
    res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[0-9]+% CI"))

    if (nrow(res_CI) > 0) {

      CI_pct <- dplyr::pull(res_CI, statistic) %>%
        dplyr::first() %>%
        readr::parse_number()

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
