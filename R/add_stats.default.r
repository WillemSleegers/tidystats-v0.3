#' add_stats generic function
#'
#' \code{add_stats} is a generic function to add a line of tidy stats to a tidy stats data.frame.
#'
#' @param output output of a statistical test.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", or "other", can be abbreviated.
#' @param description a character string to add additional information.
#'
#' @examples
#' results <- list()
#' model <- t.test(1:10, y = c(7:20))
#' results <- add_stats(model, results, identifier = "M1", type = "hypothesis")
#'
#' @import dplyr
#'
#' @export
add_stats.default <- function(output, results, identifier = NULL, statistics = NULL, type = NULL,
                              confirmatory = NULL, notes = NULL) {

  # Create an identifier if it is not specified, else check whether it already exists
  if (is.null(identifier)) {
    identifier <- paste0("M", formatC(length(results)+1, width = "1", format = "d"))
  } else {
    if (!is.null(names(results))) {
      if (identifier %in% names(results)) {
        stop("Identifier already exists.")
      }
    }
  }

  # Create the new element
  new_element <- tidy_stats(output)

  # Add the type
  if (!is.null(type)) {
    type <- match.arg(type, choices = c("hypothesis", "manipulation check", "contrast",
                                        "descriptives", "other"))

    new_element$type <- case_when(
      substr(type, 1, 1) == "h" ~ "hypothesis",
      substr(type, 1, 1) == "m" ~ "manipulation check",
      substr(type, 1, 1) == "c" ~ "contrast",
      substr(type, 1, 1) == "d" ~ "descriptives",
      TRUE ~ NA_character_
    )
  }

  # Add information whether the analysis was confirmatory or not
  if (!is.null(confirmatory)) {
    new_element$confirmatory <- case_when(
      confirmatory == TRUE ~ "yes",
      confirmatory == FALSE ~ "no",
      TRUE ~ NA_character_
    )
  }

  # Add notes
  if (!is.null(notes)) {
    new_element$notes <- notes
  }

  # Add the new element to the list
  results[[identifier]] <- new_element

  # Return the new results list
  return(results)
}
