#' Count the total of observations
#'
#' \code{count_data} returns the number of observations for categorical
#' variables.
#'
#' @param data A data frame.
#' @param ... One or more unquoted (categorical) column names from the data
#' frame, separated by commas.
#' @param na.rm Logical. Should missing values (including NaN) be removed?
#'
#' @details The data frame can be grouped using \strong{dplyr}'s \code{group_by}
#' so that the total of observations will be calculated for each group level.
#'
#' @examples
#' library(dplyr)
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
#' @export
count_data <- function(data, ..., na.rm = TRUE) {

  # Get variables
  vars <- quos(...)

  if (length(vars) > 0) {
    # Check whether all variables are non-numeric
    if (sum(!sapply(data[, stringr::str_replace(as.character(vars), "~", "")], class) %in%
            c("factor", "character")) > 0) {
      stop("Variables contain unsupported variable type")
    }

    # Get grouping
    grouping <- dplyr::group_vars(data)

    # Select all relevant columns from the data frame
    output <- dplyr::select(data, !!! vars, group_vars(data))

    # Convert all factors to characters
    output <- dplyr::mutate_if(output, is.factor, as.character)

    # Gather the requested variables into a single columns
    output <- tidyr::gather(output, "var", "group", !!! vars)

    # Re-order columns
    output <- dplyr::select(output, var, group, everything())

    # Re-group the data frame
    output <- output %>%
      dplyr::ungroup() %>%
      dplyr::group_by_all()

    # Calculate descriptives
    output <- dplyr::summarize(output, n = n())

    # Calculate percentage of each group per var
    output <- output %>%
      dplyr::group_by(var) %>%
      dplyr::mutate(pct = n / sum(n) * 100)

    # Group by original grouping
    output <- dplyr::group_by_at(output, vars(grouping))
  } else {
    # TODO: When the data is grouped, return N for each group
    output <- data_frame(
      N = nrow(data)
    )
  }

  return(output)
}