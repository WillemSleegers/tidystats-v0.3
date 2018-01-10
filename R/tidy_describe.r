#' Convert descriptives to a tidy data frame
#'
#' \code{tidy_describe} returns a tidy data frame of descriptive statistics created with \strong{tidystats}' \code{describe}.
#'
#' @param descriptives A descriptives data frame created with tidystats' \code{describe}.
#'
#' @examples
#' library(dplyr)
#'
#' # Get descriptives
#' descriptives <- describe(sleep, extra)
#'
#' # Create a tidy data frame of the descriptives
#' tidy_describe(descriptives)
#'
#' # With a grouping variable:
#' sleep %>%
#'   group_by(group) %>%
#'   describe(extra) %>%
#'   tidy_describe()
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_describe <- function(descriptives) {

  # Retrieve grouping information
  groups <- dplyr::group_vars(descriptives)

  # Gather the data
  if (length(groups) > 0) {
    output <- descriptives %>%
      tidyr::gather("statistic", "value", -var, -one_of(groups)) %>%
      dplyr::arrange(.by_group = TRUE) %>%
      tidyr::unite(col = "group", groups, sep = " - ")
  } else {
    output <- tidyr::gather(descriptives, "statistic", "value", -var)
  }

  # Arrange by var
  output <- select(output, everything()) %>%
    arrange(var)

  return(output)
}
