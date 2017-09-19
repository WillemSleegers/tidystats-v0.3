#' Calculate common descriptive statistics
#'
#' \code{describe} returns a set of common descriptive statistics for a numeric variable (e.g., n, mean, sd) or for a non-numeric variable (e.g., n, pct).
#'
#' @param data A data frame
#' @param variable The variable you want to calculate the descriptives of
#' @param na.rm Boolean to indicate whether missing data should be removed. The default is TRUE.
#'
#' @details
#' \code{describe} returns an ungrouped data frame.
#'
#' @examples
#' # Descriptives of a single variable
#' describe(sleep, variable = extra)
#' describe(sleep, group)
#'
#' # Descriptives of a single variable per group
#' sleep %>%
#'   group_by(group) %>%
#'   describe(sleep, variable = extra)
#'
#' @import dplyr
#'
#' @export
describe <- function(data, variable, na.rm = TRUE) {

  var <- enquo(variable)

  # Save grouping data
  groups <- attr(data, "vars")

  # Check variable type
  if (sapply(data, class)[quo_name(var)] %in% c("numeric", "integer")) {

    # Calculate descriptives
    output <- data %>%
      summarize(
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

  } else if (sapply(data, class)[quo_name(var)] %in% c("factor", "character", "logical")) {

    # Group the data
    if (length(groups) > 0) {
      data <- group_by(data, !!var, add = TRUE)
    } else {
      data <- group_by(data, !!var)
    }

    # Calculate frequencies
    output <- data %>%
      summarize(
        n   = n()
      )

    # Add percentages when grouping variables are provided
    if (length(groups) > 0) {
      output <- output %>%
        mutate(pct_group = n / sum(n)*100) %>%
        ungroup() %>%
        mutate(pct = n / sum(n)*100) %>%
        select(-pct_group, pct_group) # Move the variable to the last position
    } else {
      output <- output %>%
        mutate(pct = n / sum(n)*100)
    }

  } else {
    stop("Non-supported variable type.")
  }

  # Ungroup output
  output <- ungroup(output)

  return(output)
}