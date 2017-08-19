#' Calculate frequency statistics
#'
#' \code{frequencies} returns a set of common frequency statistics (e.g., n, percentage).
#'
#' @param data A data frame
#' @param ... A variable number of grouping variables. If provided, descriptives will be calculated
#' for each level of the grouping variables
#' @param na.rm Boolean to indicate whether missing data should be removed. The default is TRUE.
#'
#' @examples
#' frequencies(sleep, group)
#'
#' @import dplyr
#' @importFrom tidyr unite
#'
#' @export
frequencies <- function(data, ..., na.rm = TRUE) {

  group_by <- quos(...)

  # Calculate frequencies
  output <- data %>%
    group_by(!!!group_by) %>%
    summarize(
      n = n()
    ) %>%
    mutate(percentage = n / sum(n)*100)

  # Combine all grouping variables into 'group'
  if (length(group_by) > 0) {
    output <- output %>%
      unite(group, !!!group_by)
  } else {
    output <- output %>%
      rename(group = !!!group_by)
  }

  return(as_data_frame(output))
}

