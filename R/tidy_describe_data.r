#' Convert descriptives to a tidy data frame
#'
#' \code{tidy_describe_data} returns a tidy data frame of descriptive statistics created with \strong{tidystats}' \code{describe_data}.
#'
#' @param descriptives A data frame created with tidystats' \code{describe_data}.
#'
#' @examples
#' library(dplyr)
#'
#' # Calculate descriptives
#' descriptives <- describe_data(sleep, extra)
#'
#' # Create a tidy data frame of the descriptives
#' tidy_describe_data(descriptives)
#'
#' # With a grouping variable:
#' sleep %>%
#'   group_by(group) %>%
#'   describe_data(extra) %>%
#'   tidy_describe_data()
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_describe_data <- function(descriptives) {

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
