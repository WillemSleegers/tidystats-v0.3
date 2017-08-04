#' Convert a tidystats list to a data frame
#'
#' \code{list_to_df} converts a tidystats list to a data frame
#'
#'@examples
#' list_to_df(results)
#'
#'@import dplyr
#'
#'@export
list_to_df <- function(results) {
  df <- bind_rows(results)
  df$identifier <- names(results)
  df <- select(df, identifier, everything())

  return(df)
}
