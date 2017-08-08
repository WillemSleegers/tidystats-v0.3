#' Write a csv file with all stats
#'
#' \code{write_stats} creates a \code{csv} file with all stats
#'
#' @examples
#' results <- new_stats_list()
#' model <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, model, "model1", "hypothesis")
#' write_stats(results, "results.csv)
#'
#' @import readr
#' @import dplyr
#'
#' @export
write_stats <- function(results, path) {
  df <- stats_list_to_df(results)

  write_csv(df, path = path, na = "")
}
