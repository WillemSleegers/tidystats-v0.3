#' tidy_descriptives Function
#'
#' \code{tidy_descriptives} returns a tidy data frame of descriptive statistics.
#'
#' @examples
#' results <- descriptives(sleep, group)
#' tidy_descriptives(results)
#'
#' @import dplyr
#' @export
tidy_descriptives <- function(descriptives) {

  # Gather the data
  if (!"group" %in% names(descriptives)) {
    output <- gather(descriptives, "descriptive", "value", -var)
  } else {
    output <- descriptives %>%
      gather("descriptive", "value", -var, -group) %>%
      arrange(group)
  }

  return(output)
}
