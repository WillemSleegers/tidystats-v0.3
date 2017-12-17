#' Save the results in a tidy stats list to a .csv file
#'
#' \code{write_stats} converts a tidy stats results list to a data frame and then saves the data to a .csv file.
#'
#' @param results A tidy stats list.
#' @param path Path or connection to write to.
#' @param digits How many significant digits are to be used for rounding. Default is 3.
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
#' dir <- tempdir()
#' write_stats(results, file.path(dir, "results.txt"))
#'
#' @import readr
#'
#' @export

write_stats <- function(results, path, digits = 3) {

  # Convert list to a data frame
  df <- stats_list_to_df(results)

  # Round the stats values
  df$value <- prettyNum(df$value)

  # Write to disk
  readr::write_csv(df, path = path, na = "")
}
