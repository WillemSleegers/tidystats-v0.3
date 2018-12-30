#' Save the results in a tidy stats list to a .csv file
#'
#' \code{write_stats} converts a tidy stats results list to a data frame and then saves the data to a .csv file.
#'
#' @param results A tidy stats list.
#' @param path Path or connection to write to.
#'
#' @details The \code{prettyNum} function is used to format the numbers before saving them to disk. This is to prevent saving numbers with many decimals.
#'
#' @examples
#' library(dplyr)
#'
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct statistical tests
#' model1 <- t.test(anxiety ~ condition, data = cox)
#' model2 <- cor.test(cox$anxiety, cox$avoidance)
#'
#' # Add output to the results list
#' results <- results %>%
#'   add_stats(model1, identifier = "M1") %>%
#'   add_stats(model2, identifier = "M2")
#'
#' # Save the results
#' dir <- tempdir()
#' write_stats(results, file.path(dir, "results.txt"))
#'
#' @export

write_stats <- function(results, path) {

  # Check whether the arguments are supplied
  if (!is.list(results)) {
    stop("argument 'results' is not a list")
  }

  if (is.null(path)) {
    stop()
  }

  # Convert list to a data frame
  df <- stats_list_to_df(results)

  # Round the stats values
  df <- mutate(df, value = prettyNum(value))

  # Write to disk
  readr::write_csv(df, path = path, na = "")
}
