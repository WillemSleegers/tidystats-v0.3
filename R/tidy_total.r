#' Convert count descriptives to a tidy data frame
#'
#' \code{tidy_total} returns a tidy data frame of count statistics created with \strong{tidystats}' \code{total}.
#'
#' @param descriptives a descriptives data frame created with tidystats' \code{total}.
#'
#' @examples
#' library(dplyr)
#'
#' # Get descriptives
#' condition_n <- total(cox, condition)
#'
#' # Create a tidy data frame of the descriptives
#' tidy_total(condition_n)
#'
#' # With a grouping variable:
#' cox %>%
#'   group_by(sex) %>%
#'   describe(condition) %>%
#'   tidy_total()
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_total <- function(descriptives) {

  # Retrieve grouping information
  groups <- group_vars(descriptives)

  # Gather the data
  if (length(groups) > 0) {
    output <- descriptives %>%
      gather("statistic", "value", -var, -group, -one_of(groups)) %>%
      arrange(.by_group = TRUE) %>%
      unite(col = "group", group, groups, sep = " - ")
  } else {
    output <- gather(descriptives, "statistic", "value", -var, -group)
  }

  # Arrange by var
  output <- arrange(output, var, group)

  return(output)
}
