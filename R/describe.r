#' describe function
#'
#' \code{describe} is a function to produce descriptives.
#'
#' @examples
#' describe(sleep, extra)
#' describe(sleep, extra, group = group)
#'
#' @import dplyr
#' @import tidyr
#' @import lazyeval
#'
#' @export
describe <- function(data, variable, ..., na.rm = TRUE) {

  # Group the data, if group variables are provided
  group_by <- quos(...)

  if (length(group_by) > 0) {
    data <- data %>%
      unite(group, !!!group_by) %>%
      group_by(group)
  }

  # Calculate descriptives
  var <- enquo(variable)

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

  # Return the descriptives
  return(output)
}