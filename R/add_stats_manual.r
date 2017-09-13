#' Manually add stats to tidy results list
#'
#' \code{add_stats_manual} is a function to add a tidy data frame of statistical results to a tidy stats list. tidystats does not support all possible statistical tests, so it may not be able to produce tidy output of a statistical model. The best solution for now is to tidy the output of a statistical method yourself, and then use this function to add it to the tidy stats list.
#'
#' @param data output of a statistical test in a tidy data frame format.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", or "other", can be abbreviated.
#' @param description a character string to add additional information.
#'
#' @import dplyr
#'
#' @export
add_stats_manual <- function(data, results, identifier = NULL, type = NULL, description = NULL) {

  # Create an identifier if it is not specified
  # If one is specified, check whether it already exists
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
  new_element <- data

  # Maybe add a check for certain columns?

  # Add the type
  if (!is.null(type)) {
    type <- match.arg(type, choices = c("hypothesis", "manipulation check", "contrast", "other"))

    new_element$type <- case_when(
      substr(type, 1, 1) == "h" ~ "hypothesis",
      substr(type, 1, 1) == "m" ~ "manipulation check",
      substr(type, 1, 1) == "c" ~ "contrast",
      TRUE ~ NULL
    )
  }

  # Add the new element to the list
  results[[identifier]] <- data

  # Return the new results list
  return(results)
}
