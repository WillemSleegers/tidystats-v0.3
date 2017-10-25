#' Save the results in a tidy stats list to a .csv file
#'
#' \code{write_stats} converts a tidy stats results list to a data frame and then saves the data to a .csv file.
#'
#' @param results A tidy stats list.
#' @param path Path or connection to write to.
#'
#' @examples
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct statistical tests
#' model <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' # Add output to the results list
#' results <- add_stats(model, results, identifier = "M1")
#'
#' # Save the results
#' write_stats(results, "results.csv")
#'
#' @import readr
#'
#' @export

write_stats <- function(results, path) {
  df <- stats_list_to_df(results)

  readr::write_csv(df, path = path, na = "")
}
