#' Convert a tidy stats list to a data frame
#'
#' \code{stats_list_to_df} converts a tidy stats list to a data frame.
#'
#' @param results A tidy stats list.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
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
    purrr::map2_df(names(results), add_identifier) %>%
    dplyr::select(identifier, contains("term_nr"), contains("term"), everything(),
           -contains("notes"), contains("notes"))

  return(df)
}
