#' Write a .csv file with all stats
#'
#' \code{write_stats} creates a \code{.csv} file with all stats

#' @examples
#' results <- new_stats_data_frame()
#' ttest <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y")

#'@import dplyr

#'@export
read_stats <- function(file) {
  df <- read.csv(file, na.strings = "")
  results <- split(df, df$identifier)
  return(results)
}
