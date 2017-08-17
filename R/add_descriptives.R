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
add_descriptives <- function(descriptives, list, identifier, group = NULL, description = NULL) {

  # Check if the identifier already exists
  if (identifier %in% names(list)) {
    stop("Identifier already exists.")
  }

  # Create an identifier if it is not specified
  if (is.null(identifier)) {
    identifier <- formatC(nrow(list), width = "2", format = "d", flag = "0") %>%
      paste0("D", .)
  }

  # Gather the data
  if (is.null(group)) {
    descriptives <- gather(descriptives, "descriptive", "value")
  } else {
    descriptives <- rename(descriptives, "group" = group) %>%
      gather("descriptive", "value", -group)
  }

  # Add the description, if given
  if (!is.null(description)) {
    descriptives$description <- description
  }

  # Add descriptives to the list
  list[[identifier]] <- descriptives

  # Return the new results data.frame
  return(list)
}
