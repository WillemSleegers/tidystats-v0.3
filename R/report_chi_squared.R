#' Report function for a chi-squared test
#'
#' Function to report a chi-squared test in APA style.
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
#' report(results, identifier = "chi_square")
#' report(results, identifier = "chi_square", statistic = "p")
#'
#' @export

report_chi_squared <- function(results, identifier, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Check whether a single statistic is requested, or a full line of APA output
  if (!is.null(statistic)) {
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    } else {
      res_statistic = statistic
    }

    value <- pull(filter(res, statistic == res_statistic), value)
    output <- report_statistic(res_statistic, value)
  } else {
    x_squared <- pull(filter(res, statistic == "X-squared"), value)
    df        <- pull(filter(res, statistic == "df"), value)
    p         <- pull(filter(res, statistic == "p"), value)

    x_squared <- report_statistic("X-squared", x_squared)
    df        <- report_statistic("df", df)
    p         <- report_p_value(p)

    output <- paste0("$\\chi^2$(", df, ") = ", x_squared, ", ", p)
  }

  return(output)
}
