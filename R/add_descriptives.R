#' add_descriptives Function
#'
#' \code{add_descriptives} is a generic function to add descriptive information to a tidy descriptives data frame.

#' @examples
#' descriptives <- new_descriptives_data_frame()
#'
#' # Use Student's sleep data
#' sleep %>%
#'   group_by(group) %>%
#'   summarize(
#'     mean = mean(extra),
#'     sd = sd(extra),
#'     n = n()
#'   ) -> sleep_descriptives
#'
#' descriptives <- add_descriptives(descriptives, sleep_descriptives,
#'                                  identifier = "D1", outcome_variable = "extra")

#'@import dplyr

#'@export
add_descriptives <- function(descriptives, new_descriptives, identifier, outcome_variable = NULL) {

  # Check if the identifier already exists
  if (identifier %in% descriptives$identifier) {
    stop("Identifier already exists.")
  }

  # Add information to new descriptives data frame
  if (is.null(outcome_variable)) {
    temp <- new_descriptives %>%
      mutate(identifier = identifier) %>%
      select(identifier, everything())
  } else {
    temp <- new_descriptives %>%
      mutate(
        identifier = identifier,
        outcome = outcome_variable
      ) %>%
      select(identifier, outcome, everything())
  }

  # Add the new descriptives to the full descriptives data frame
  descriptives <- bind_rows(descriptives, temp)

  # Return the new results data.frame
  return(descriptives)
}
