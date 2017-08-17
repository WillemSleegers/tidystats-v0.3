#' Write a csv file containing descriptives
#'
#' \code{write_descriptives} creates a .csv file containing descriptives
#'
#' @examples
#' descriptives <- list()
#'
#' descriptives <- sleep %>%
#'   describe(variable = "measure", group = "group") %>%
#'   add_descriptives(descriptives, identifier = "sleep_data")
#'
#' write_descriptives(descriptives, "descriptives.csv")
#'
#' @import readr
#'
#' @export
write_descriptives <- function(descriptives, path) {
  df <- descriptives_list_to_df(descriptives)

  write_csv(df, path = path, na = "")
}
