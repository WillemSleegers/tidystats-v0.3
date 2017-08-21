#' Convert a tidystats descriptives list to a data frame
#'
#' \code{descriptives_list_to_df} converts a tidystats descriptives list to a data frame
#'
#' @examples
#' descriptives_list_to_df(descriptives)
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map2_df
#'
#' @export
descriptives_list_to_df <- function(descriptives) {

  # Create function to add the identifier to the stats data frame
  add_identifier <- function(x, y) {
    x$identifier <- y

    return(x)
  }

  # Merge the stats output together and reorder variables
  df <- descriptives %>%
    map2_df(names(descriptives), add_identifier) %>%
    select(identifier, one_of("var", "group"), everything())

  return(df)
}
