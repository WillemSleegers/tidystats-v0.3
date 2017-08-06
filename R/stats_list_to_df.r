#' Convert a tidystats list to a data frame
#'
#' \code{stats_list_to_df} converts a tidystats list to a data frame
#'
#' @examples
#' stats_list_to_df(results)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import purrr
#'
#' @export
stats_list_to_df <- function(results) {

  # Loop over each element in the list and add the identifier information, then reorder columns
  df <- results %>%
    map2_df(names(results), mutate, identifier = `.y[[i]]`) %>%
    select(identifier, method, one_of("term"), everything(), estimate, statistic, df_model,
           df_error, -`.y[[i]]`)

  return(df)
}
