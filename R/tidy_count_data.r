#' Convert count data to a tidy data frame
#'
#' \code{tidy_count_data} returns a tidy data frame of count statistics created with \strong{tidystats}' \code{count_data}.
#'
#' @param count_data a data frame created with tidystats' \code{count_data}.
#'
#' @examples
#' library(dplyr)
#'
#' # Calculate counts
#' condition_n <- count_data(cox, condition)
#'
#' # Create a tidy data frame of the count data
#' tidy_count_data(condition_n)
#'
#' # With a grouping variable:
#' cox %>%
#'   group_by(sex) %>%
#'   count_data(condition) %>%
#'   tidy_count_data()
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_count_data <- function(count_data) {

  # Retrieve grouping information
  groups <- group_vars(count_data)

  # Gather the data
  if (length(groups) > 0) {
    output <- count_data %>%
      gather("statistic", "value", -var, -group, -one_of(groups)) %>%
      arrange(.by_group = TRUE) %>%
      unite(col = "group", group, groups, sep = " - ")
  } else {
    output <- gather(count_data, "statistic", "value", -contains("var"), -contains("group"))

    # Arrange by var, if it exists
    if ("var" %in% names(output)) {
      output <- arrange(output, var, group)
    }
  }

  return(output)
}
