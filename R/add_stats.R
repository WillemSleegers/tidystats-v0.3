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

  # Create an identifier if it is not specified
  if (is.null(identifier)) {
    identifier <- formatC(nrow(results), width = "2", format = "d", flag = "0") %>%
      paste("Model", .)
  }

  # Create the new row
  new_row <- tidystats(model, identifier, type, description) %>%
    mutate_at(vars(identifier, method, description), as.character)

  # Check if the identifier already exists
  if (identifier %in% results$identifier) {
    stop("Identifier already exists.")
  }

  # Add the new row to the data.frame
  new_results <- bind_rows(results, new_row)

  # Return the new results data.frame
  return(new_results)
}
