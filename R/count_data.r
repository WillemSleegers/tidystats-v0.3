#' Count the total of observations
#'
#' \code{count_data} returns the number of observations for categorical variables.
#'
#' @param data a data frame.
#' @param variables the variables you want to count.
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by} so that the total of observations will be calculated for each group level.
#'
#' @examples
#' library(tidyverse)
#'
#' # 1 variable
#' count_data(cox, sex)
#'
#' # 2 variables
#' count_data(cox, condition, sex)
#'
#' # 1 variable, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   count_data(sex)
#'
#' @import dplyr
#'
#' @export
count_data <- function(data, ..., na.rm = TRUE) {

  # Get variables
  vars <- quos(...)

  # Check whether all variables are non-numeric
  if (sum(!sapply(data[, str_replace(as.character(vars), "~", "")], class) %in%
          c("factor", "character")) > 0) {
    stop("Variables contain unsupported variable type")
  }

  # Get grouping
  grouping <- group_vars(data)

  # Select all relevant columns from the data frame
  output <- select(data, !!! vars, group_vars(data))

  # Convert all factors to characters
  output <- mutate_if(output, is.factor, as.character)

  # Gather the requested variables into a single columns
  output <- gather(output, "var", "group", !!! vars)

  # Re-order columns
  output <- select(output, var, group, everything())

  # Re-group the data frame
  output <- output %>%
    ungroup %>%
    group_by_all()

  # Calculate descriptives
  output <- output %>%
    dplyr::summarize(
      n       = n()
    )

  # Calculate percentage of each group per var
  output <- output %>%
    group_by(var) %>%
    mutate(pct = n / sum(n) * 100)

  # Group by original grouping
  output <- group_by_at(output, vars(grouping))

  return(output)
}