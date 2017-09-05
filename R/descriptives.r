#' Calculate common descriptive statistics
#'
#' \code{descriptives} returns a set of common descriptive statistics (e.g., n, mean, sd).
#'
#' @param data A data frame
#' @param variable The variable you want to calculate the descriptives of
#' @param ... A variable number of grouping variables. If provided, descriptives will be calculated
#' for each level of the grouping variables
#' @param na.rm Boolean to indicate whether missing data should be removed. The default is TRUE.
#'
#' @examples
#' descriptives(sleep, extra)
#' descriptives(sleep, extra, group)
#'
#' @import dplyr
#' @importFrom tidyr unite
#'
#' @export
descriptives <- function(data, variable, ..., na.rm = TRUE) {

  var <- enquo(variable)

  # Check whether the variable is numeric
  if (!class(pull(select(data, !!var))) %in% c("numeric","integer")) {
    stop(paste("non-numeric variable."))
  }

  # Group the data, if group variables are provided
  group_by <- quos(...)

  if (length(group_by) > 0) {
    data <- data %>%
      group_by(!!!group_by)
  }

  # Calculate descriptives
  output <- data %>%
    summarize(
      var     = paste(var)[2],
      missing = sum(is.na(!!var)),
      M       = mean(!!var, na.rm = na.rm),
      SD      = sd(!!var, na.rm = na.rm),
      min     = min(!!var, na.rm = na.rm),
      max     = max(!!var, na.rm = na.rm),
      range   = diff(range(!!var, na.rm = na.rm)),
      median  = median(!!var, na.rm = na.rm),
      mode    = unique(!!var)[which.max(tabulate(match(!!var, unique(!!var))))],
      n       = n() - missing,
      SE      = SD/sqrt(n)
    )

  # Combine the grouping variables into 'group', if provided
  if (length(group_by) > 0) {
    output <- output %>%
      unite(group, !!!group_by, sep = ", ")
  }

  # Reorder variables
  output <- output %>%
    select(var, one_of(group), missing, n, M, SD, SE, everything())

  return(as_data_frame(output))
}