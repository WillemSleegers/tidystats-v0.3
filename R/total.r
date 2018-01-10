#' Calculate the total of observations
#'
#' \code{total} returns the number of observations for categorical variables.
#'
#' @param data a data frame.
#' @param variables the variables you want to total.
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by} so that the total of observations will be calculated for each group level.
#'
#' @examples
#' library(tidyverse)
#'
#' # 1 variable
#' total(cox, sex)
#'
#' # 1 variable, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   describe(sex)
#'
#' @import dplyr
#'
#' @export
total <- function(data, ..., na.rm = TRUE) {

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
  data <- select(data, !!! vars, group_vars(data))

  # Gather the requested variables into a single columns
  data <- gather(data, "var", "group", !!! vars)

  # Re-order columns
  data <- select(data, var, group, everything())

  # Re-group the data frame
  data <- data %>%
    ungroup %>%
    group_by_all()

  # Calculate descriptives
  output <- data %>%
    dplyr::summarize(
      n       = n()
    )

  # Calculate percentage of each group per var
  output <- output %>%
    group_by(var) %>%
    mutate(pct = n / sum(n) * 100)

  # Ungroup output
  output <- ungroup(output)

  return(output)
}