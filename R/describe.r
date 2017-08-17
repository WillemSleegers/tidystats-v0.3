#' describe function
#'
#' \code{describe} is a function to produce descriptives.
#'
#' @examples
#' describe(sleep, extra)
#' describe(sleep, extra, group = group)
#'
#' @import dplyr
#' @import lazyeval
#'
#' @export
describe <- function(data, variable, group = NULL, na.rm = TRUE) {

  # Group the data by the group argument, if provided, and rename to 'group'
  if (!is.null(group)) {
    data <- data %>%
      rename_(`group` = group) %>%
      group_by(group)
  }

  # Calculate descriptives
  output <- data %>%
    summarize_(
      missing = interp(~sum(is.na(x)), x = as.name(variable)),
      n = ~n() - missing,
      M = interp(~mean(x, na.rm = na.rm), x = as.name(variable)),
      SD = interp(~sd(x, na.rm = na.rm), x = as.name(variable)),
      SE = ~(SD/sqrt(n)),
      min = interp(~min(x, na.rm = na.rm), x = as.name(variable)),
      max = interp(~max(x, na.rm = na.rm), x = as.name(variable)),
      range = interp(~diff(range(x, na.rm = na.rm)), x = as.name(variable)),
      median = interp(~median(x, na.rm = na.rm), x = as.name(variable)),
      mode = interp(~unique(x)[which.max(tabulate(match(x, unique(x))))], x = as.name(variable))
    )

  # Return the descriptives
  return(output)
}
