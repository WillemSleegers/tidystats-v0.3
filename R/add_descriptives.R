#' add_descriptives Function
#'
#' \code{add_descriptives} is a generic function to add descriptive information to a tidy descriptives data frame.
#'
#' @examples
#' descriptives <- list()
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
#'                                  identifier = "D1")
#'
#' @import dplyr
#' @export
add_descriptives <- function(descriptives, list, identifier = NULL, description = NULL) {

  # Create an identifier if it is not specified, else check whether it already exists
  if (is.null(identifier)) {
    identifier <- paste0("D", formatC(length(list)+1, width = "1", format = "d"))
  } else {
    if (identifier %in% names(list)) {
      stop("Identifier already exists.")
    }
  }

  # Tidy the data
  output <- tidy_descriptives(descriptives)

  # Add the description, if given
  if (!is.null(description)) {
    output$description <- description
  }

  # Add descriptives to the list
  list[[identifier]] <- output

  # Return the new results data.frame
  return(list)
}
