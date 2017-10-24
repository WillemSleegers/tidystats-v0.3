#' Convert a tidy stats list to a data frame
#'
#' \code{stats_list_to_df} converts a tidy stats list to a data frame.
#'
#' @examples
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct statistical tests
#' model <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' # Add output to the results list
#' results <- add_stats(model, results, identifier = "M1")
#'
#' # Convert list to a data frame
#' stats_list_to_df(results)
#'
#' @import dplyr
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
    select(identifier, contains("term_nr"), contains("term"), everything(),
           -contains("notes"), contains("notes"))

  return(df)
}
