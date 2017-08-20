#' tidy_frequencies Function
#'
#' \code{tidy_frequencies} returns a tidy data frame of frequency statistics.
#'
#' @examples
#' results <- frequencies(sleep, group)
#' tidy_frequencies(results)
#'
#' @import dplyr
#' @export
tidy_frequencies <- function(frequencies) {

  # Gather the data
  if (!"group" %in% names(frequencies)) {
    output <- gather(frequencies, "descriptive", "value", -var)
  } else {
    output <- frequencies %>%
      gather("descriptive", "value", -var, -group) %>%
      arrange(group)
  }

  return(output)
}
