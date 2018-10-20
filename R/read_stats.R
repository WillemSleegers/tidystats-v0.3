#' Read a .csv file that was produced with \code{write_stats}
#'
#' \code{read_stats} can convert a .csv file containing the statistical results that was produced with \code{write_stats}. It returns a list containing the results, with the identifier as the name for each list element.
#'
#' @param file Path to the tidy stats data file
#'
#' @examples
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' @import readr
#' @import purrr
#'
#' @export

read_stats <- function(file) {

  # Read a tidystats csv file
  df <- readr::read_csv(file)

  # Split the data frame by the identifier to create a list
  results <- split(df, df$identifier)

  # Loop over each element and remove columns that are empty
  empty_column <- function(x) {
    sum(!is.na(x)) > 0
  }

  results <- purrr::map(results, dplyr::select_if, empty_column)

  # Loop over each element and remove the identifier column
  results <- purrr::map(results, dplyr::select, -identifier)

  return(results)
}
