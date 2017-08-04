#' Convert a tidystats list to a data frame
#'
#' \code{list_to_df} converts a tidystats list to a data frame
#'
#'@examples
#' list_to_df(results)
#'
#'@import dplyr
#'@import purrr
#'
#'@export
list_to_df <- function(results) {

  # Loop over each element in the list and add the identifier information, then reorder
  results %>%
    map2_df(names(results), mutate, identifier = `.y[[i]]`) %>%
    select(identifier, method, term, everything(), -`.y[[i]]`) -> temp

  return(df)
}
