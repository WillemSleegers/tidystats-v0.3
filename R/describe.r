#' Calculate common descriptive statistics
#'
#' \code{describe} returns a set of common descriptive statistics for a numeric variable (e.g., n, mean, sd) or for a non-numeric variable (e.g., n, pct).
#'
#' @param data A data frame
#' @param variable The variable you want to calculate the descriptives of
#' @param na.rm Boolean to indicate whether missing data should be removed. The default is TRUE.
#'
#' @details The data set can be grouped so that descriptives will be calculated for each group level. Unlike dplyr's \code{summarize}, \code{describe} does not automatically peel off a grouping variable. The function keeps the supplied grouping. However, when descriptives are requested of a non-numeric variable, the function will return a data frame that is grouped by that variable. If the data frame was already grouped, the non-numeric variable will be added to the existing grouping variables.
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#'
#' # Descriptives of a single variable
#' describe(sleep, variable = extra)
#' describe(sleep, variable = group)
#'
#' # Descriptives of a single variable per group
#' sleep %>%
#'   group_by(group) %>%
#'   describe(variable = extra)
#'
#' @import dplyr
#'
#' @export
describe <- function(data, variable, na.rm = TRUE) {

  var <- enquo(variable)

  # Save grouping data
  groups <- dplyr::group_vars(data)

  # Check variable type
  if (sapply(data, class)[quo_name(var)] %in% c("numeric", "integer")) {

    # Calculate descriptives
    output <- data %>%
      dplyr::summarize(
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
      data <- dplyr::group_by(data, !!var, add = TRUE)
    } else {
      data <- dplyr::group_by(data, !!var)
    }

    # Save new grouping data
    groups <- dplyr::group_vars(data)

    # Calculate frequencies
    output <- data %>%
      dplyr::summarize(
        n   = n()
      )

    # Add percentages when grouping variables are provided
    if (length(groups) > 1) {
      output <- output %>%
        dplyr::mutate(pct_group = n / sum(n)*100) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(pct = n / sum(n)*100) %>%
        dplyr::select(-pct_group, pct_group) # Move the variable to the last position
    } else {
      output <- output %>%
        dplyr::mutate(pct = n / sum(n)*100)
    }
  } else {
    stop("Non-supported variable type.")
  }

  # Add grouping to output
  output <- output %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(vars(one_of(groups)))

  return(output)
}