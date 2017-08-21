#' add_stats Generic Function
#'
#' \code{add_stats} is a generic function to add a line of tidy stats to a tidy stats data.frame.
#'
#' @param results a tidy stats list.
#' @param model output of a statistical test.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", or "other", can be abbreviated.
#' @param description a character string to add additional information.
#'
#' @examples
#' results <- list()
#' model <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, model, "M1", "hypothesis")

#'@import dplyr

#'@export
add_stats <- function(model, results, identifier = NULL, type = "other", description = NULL) {

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

  # Add the type
  type <- match.arg(type, choices = c("hypothesis", "manipulation check", "contrast", "other"))

  new_element$type <- case_when(
    substr(type, 1, 1) == "h" ~ "hypothesis",
    substr(type, 1, 1) == "m" ~ "manipulation check",
    substr(type, 1, 1) == "c" ~ "contrast",
    TRUE ~ "other"
  )

  # Add the new element to the list
  results[[identifier]] <- new_element

  # Return the new results list
  return(results)
}
