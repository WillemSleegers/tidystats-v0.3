#' Read a .csv file with all stats
#'
#' \code{read_stats} returns a list containing all stats, with the identifier as the name for each list element.
#'
#' @examples
#' results <- read_stats("results.csv")
#'
#' @import readr
#' @import dplyr
#'
#'@export
read_stats <- function(file) {
  df <- read_csv(file)
  results <- split(df, df$identifier)

  return(results)
}
