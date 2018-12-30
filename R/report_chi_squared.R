#' Report function for a chi-squared test
#'
#' Function to report a chi-squared test in APA style.
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
#' report(identifier = "chi_squared")
#' report(identifier = "chi_squared_yates")
#'
#' @export

report_chi_squared <- function(identifier,
  results = getOption("tidystats_list")) {

  output <- NULL

  # Extract model results
  res <- results[[identifier]]

  # Check if all the necessary statistics are there to produce a line of output
  if (sum(c("X-squared", "df", "p") %in% unique(res$statistic)) == 3) {
    # Extract statistics
    x_squared <- pull(filter(res, statistic == "X-squared"), value)
    df        <- pull(filter(res, statistic == "df"), value)
    p         <- pull(filter(res, statistic == "p"), value)

    x_squared <- report_statistic("X-squared", x_squared)
    df        <- report_statistic("df", df)
    p         <- report_p_value(p)

    output <- paste0("*\u03C7\u00B2* (", df, ") = ", x_squared, ", ", p)
  }

  return(output)
}
