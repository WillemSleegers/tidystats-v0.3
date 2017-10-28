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
#'
#' @export

read_stats <- function(file) {
  df <- readr::read_csv(file)
  results <- split(df, df$identifier)

  return(results)
}
