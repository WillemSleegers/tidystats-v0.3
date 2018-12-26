#' Report function for Fisher's Exact Tests for Count Data
#'
#' Function to report Fisher's Exact Tests for Count Data in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "fisher_test")
#' report(results, identifier = "fisher_hybrid")
#' report(results, identifier = "fisher_simulated_p")
#'
#' @export

report_fisher <- function(results, identifier) {

  # Extract model results
  res <- results[[identifier]]

  # Extract statistics
  p <- res$value[res$statistic == "p"]
  p <- report_p_value(p)

  if ("OR" %in% res$statistic) {
    OR <- res$value[res$statistic == "OR"]
    OR <- report_statistic("OR", OR)

    output <- paste0("*OR* = ", OR, ", ", p)
  } else {
    output <- p
  }

  # Add a confidence interval, if it exists
  res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[0-9]+% CI"))

  if (nrow(res_CI) > 0) {

    CI_pct <- readr::parse_number(first(res_CI$statistic))

    CI_lower <- res_CI$value[1]
    CI_upper <- res_CI$value[2]

    CI_lower <- report_statistic("CI", CI_lower)
    CI_upper <- report_statistic("CI", CI_upper)

    CI <- paste0(CI_pct, "% CI ", "[", CI_lower, ", ", CI_upper, "]")

    output <- paste0(output, ", ", CI)
  }

  return(output)
}
