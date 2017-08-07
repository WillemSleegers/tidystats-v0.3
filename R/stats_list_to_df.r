#' Convert a tidystats list to a data frame
#'
#' \code{stats_list_to_df} converts a tidystats list to a data frame
#'
#' @examples
#' stats_list_to_df(results)
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom purrr map2_df
#'
#' @export
stats_list_to_df <- function(results) {

  # Create function to add the identifier to the stats data frame
  add_identifier <- function(x, y) {
    x$identifier <- y

    return(x)
  }

  # Merge the stats output together and reorder variables
  df <- results %>%
    map2_df(names(results), add_identifier) %>%
    select(identifier, method, one_of("term", "estimate", "std_error"),
           statistic, one_of("df_model"), df_error, everything())

  return(df)
}
