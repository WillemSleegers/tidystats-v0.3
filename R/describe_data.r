#' Calculate common descriptive statistics
#'
#' \code{describe_data} returns a set of common descriptive statistics
#' (e.g., n, mean, sd) for numeric variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (numerical) column names from the data frame,
#' separated by commas.
#' @param na.rm Logical. Should missing values (including NaN) be removed?
#'
#' @details The data set can be grouped using \strong{dplyr}'s \code{group_by}
#' so that descriptives will be calculated for each group level.
#'
#' Skew and kurtosis are based on the \code{skewness} and \code{kurtosis}
#' functions of the \strong{moments} package (Komsta & Novomestky, 2015).
#'
#' @examples
#' library(dplyr)
#'
#' # 1 variable
#' describe_data(cox, avoidance)
#'
#' # 1 variable, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   describe_data(avoidance)
#'
#' # 2 variables
#' describe_data(cox, avoidance, anxiety)
#'
#' # 2 variables, 1 group
#' cox %>%
#'   group_by(condition) %>%
#'   describe_data(avoidance, anxiety)
#'
#' # 1 variable, 2 groups
#' cox %>%
#'   group_by(condition, sex) %>%
#'   describe_data(avoidance)
#'
#' # 2 variables, 2 groups
#' cox %>%
#'   group_by(condition, sex) %>%
#'   describe_data(avoidance, anxiety)
#'
#' @export
describe_data <- function(data, ..., na.rm = TRUE) {

  # Get variables
  vars <- dplyr::quos(...)

  # Throw an error if no vars are supplied
  if (length(vars) == 0) {
    stop("No variables supplied")
  }

  # Check whether all variables are numeric
  if (sum(!sapply(data[, stringr::str_sub(as.character(vars), start = 2)],
    class) %in% c("numeric", "integer")) > 0) {
    stop("Variables contain unsupported variable type")
  }

  # Get grouping
  grouping <- dplyr::group_vars(data)

  # Select all relevant columns from the data frame
  data <- dplyr::select(data, dplyr::group_vars(data), !!! vars)

  # If there are multiple variables, restructure them into 'var' and their
  # values into 'value'
  # Else add a 'var' column and rename the one variable to 'value'
  if (length(vars) > 1) {
    data <- tidyr::gather(data, "var", "value", !!! vars)
  } else {
    data$var <- as.character(vars[[1]])[2]
    data <- data %>%
      dplyr::ungroup() %>% # Temporary fix for a bug
      dplyr::rename_at(vars(!!! vars), ~"value")
  }

  # Re-group the data frame
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(vars(dplyr::one_of(grouping), var))

  # Calculate descriptives
  output <- data %>%
    dplyr::summarize(
      missing  = sum(is.na(value)),
      n        = dplyr::n() - missing,
      M        = mean(value, na.rm = na.rm),
      SD       = sd(value, na.rm = na.rm),
      SE       = SD / sqrt(n),
      min      = min(value, na.rm = na.rm),
      max      = max(value, na.rm = na.rm),
      range    = diff(range(value, na.rm = na.rm)),
      median   = median(value, na.rm = na.rm),
      mode     = unique(value)[which.max(tabulate(match(value,
                   unique(value))))],
      skew     = (sum((value-mean(value, na.rm = na.rm))^3, na.rm = na.rm) /
                  n) / (sum((value - mean(value, na.rm = na.rm))^2,
                  na.rm = na.rm) / n)^(3 / 2),
      kurtosis = n * sum((value-mean(value, na.rm = na.rm))^4, na.rm = na.rm)/
                  (sum((value-mean(value, na.rm = na.rm))^2, na.rm = na.rm)^2)
    )

  # Re-order output and sort by var
  output <- dplyr::select(output, var, everything()) %>%
    dplyr::arrange(var)

  return(output)
}