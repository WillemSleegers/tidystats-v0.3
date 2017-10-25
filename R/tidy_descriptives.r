#' Convert descriptives to a tidy data frame
#'
#' \code{tidy_descriptives} returns a tidy data frame of descriptive statistics.
#'
#' @param descriptives A descriptives data frame
#'
#' @examples
#'
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Get descriptives
#' results <- describe(sleep, group)
#'
#' # Create a tidy data frame of the descriptives
#' tidy_descriptives(results)
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_descriptives <- function(descriptives) {

  # Retrieve grouping information
  groups <- dplyr::group_vars(descriptives)

  # Gather the data
  if (length(groups) > 0) {
    output <- descriptives %>%
      tidyr::gather("statistic", "value", -one_of(groups)) %>%
      dplyr::arrange(.by_group = TRUE) %>%
      tidyr::unite(col = "group", groups, sep = "_")
  } else {
    output <- tidyr::gather(descriptives, "statistic", "value")
  }

  return(output)
}
