#' Report function for t-tests
#'
#' Function to report a t-test in APA style.
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
#' report(results, identifier = "t_test")
#' report(results, identifier = "t_test", statistic = "p")
#'
#' @export

report_t_test <- function(results, identifier, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Check whether a single statistic is requested, or a full line of APA output
  if (!is.null(statistic)) {
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    } else {
      res_statistic <- statistic
    }

    value  <- dplyr::pull(dplyr::filter(res, statistic == res_statistic), value)
    output <- report_statistic(res_statistic, value)
  } else {
    t  <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
    df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
    p  <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

    t  <- report_statistic("t", t)
    df <- report_statistic("df", df)
    p  <- report_p_value(p)

    output <- paste0("*t*(", df, ") = ", t, ", ", p)
  }

  return(output)
}
