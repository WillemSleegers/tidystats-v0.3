#' Calculate common descriptive statistics
#'
#' \code{describe} returns a set of common descriptive statistics (e.g., n, mean, sd).
#'
#' @param data A data frame
#' @param variable The variable you want to calculate the descriptives of
#' @param ... A variable number of grouping variables. If provided, descriptives will be calculated
#' for each level of the grouping variables
#' @param na.rm Boolean to indicate whether missing data should be removed. The default is TRUE.
#'
#' @examples
#' describe(sleep, extra)
#' describe(sleep, extra, group)
#'
#' @import dplyr
#' @importFrom tidyr unite
#'
#' @export
describe <- function(data, variable, ..., na.rm = TRUE) {

  var <- enquo(variable)

  # Check whether the variable is numeric
  if (!class(pull(select(data, !!var))) %in% c("numeric","integer")) {
    stop(paste("non-numeric variable."))
  }

  # Group the data, if group variables are provided
  group_by <- quos(...)

  if (length(group_by) > 0) {
    data <- data %>%
      unite(group, !!!group_by) %>%
      group_by(group)
  }

  # Calculate descriptives
  output <- data %>%
    summarize(
      var     = paste(var)[2],
      missing = sum(is.na(!!var)),
      n       = n() - missing,
      M       = mean(!!var, na.rm = na.rm),
      SD      = sd(!!var, na.rm = na.rm),
      SE      = SD/sqrt(n),
      min     = min(!!var, na.rm = na.rm),
      max     = max(!!var, na.rm = na.rm),
      range   = diff(range(!!var, na.rm = na.rm)),
      median  = median(!!var, na.rm = na.rm),
      mode    = unique(!!var)[which.max(tabulate(match(!!var, unique(!!var))))]
    )

  # Reorder columns
  output <- select(output, var, everything())

  return(output)
}