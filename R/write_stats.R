#' Write a csv file with all stats
#'
#' \code{write_stats} creates a .csv file with all stats
#'
#' @examples
#' results <- list()
#' model <- t.test(1:10, y = 7:20)
#' results <- add_stats(results, model)
#' write_stats(results, "results.csv")
#'
#' @import readr
#'
#' @export
write_stats <- function(results, path) {
  df <- stats_list_to_df(results)

  write_csv(df, path = path, na = "")
}
