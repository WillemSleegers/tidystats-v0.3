#' Report method for a chi-square test
#'
#' Function to report a chi-square test in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Example: chi-square
#' report(results, identifier = "chi_square")
#' report(results, identifier = "chi_square", statistic = "p")
#'
#' @export

report_chi_squared <- function(results, identifier, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Check whether the statistic exists, if provided
  if (!is.null(statistic)) {
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    }
  }

  # Check if only a single statistic is asked; if not, produce a full line of
  # APA results
  if (!is.null(statistic)) {
    value <- res$value[res$statistic == statistic]
    output <- report_statistic(statistic, value)
  } else {
    x_squared <- pull(filter(res, statistic == "X-squared"), value)
    df        <- pull(filter(res, statistic == "df"), value)
    p         <- pull(filter(res, statistic == "p"), value)

    x_squared <- report_statistic("X-squared", x_squared)
    df        <- report_statistic("df", df)
    p         <- report_statistic("p", p)

    output <- paste0("$\\chi^2$(", df, ") = ", x_squared, ", ", p)
  }

  return(output)
}
