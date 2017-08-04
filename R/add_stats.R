#' add_stats Generic Function
#'
#' \code{add_stats} is a generic function to add a line of tidy stats to a tidy stats data.frame.

#' @examples
#' results <- new_stats_data_frame()
#' ttest <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y")

#'@import dplyr

#'@export
add_stats <- function(results, model, identifier = NULL, type = "other", description = NULL) {

  # Check if the identifier already exists
  if (identifier %in% names(results)) {
    stop("Identifier already exists.")
  }

  # Create an identifier if it is not specified
  if (is.null(identifier)) {
    identifier <- formatC(nrow(results), width = "2", format = "d", flag = "0") %>%
      paste0("M", .)
  }

  # Create the new element
  new_element <- tidy_stats(model)

  # Add the new element to the list
  results[[identifier]] <- new_element

  # Return the new results list
  return(results)
}
