#' tidystats method for adding a tidy data frame to a tidy results list
#'
#' \code{add_stats.data.frame} is a function to add a tidy data frame of results to a tidy stats list. tidystats does not support all possible statistical tests, so it may not be able to produce tidy output of a statistical model. The best solution for now is to tidy the output of a statistical method yourself, creating a tidy data frame, and then use \code{add_stats}, which will call this function, to add it to the tidy stats list.
#'
#' @param data output of a statistical test in a tidy data frame format.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", "descriptives", or "other", can be abbreviated.
#' @param confirmatory was the analysis confirmatory? TRUE, FALSE, or NULL.
#' @param notes a character string to add additional information.
#'
#' @import dplyr
#'
#' @export
add_stats.data.frame <- function(output, results, identifier = NULL, statistics = NULL, type = NULL,
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

  # Throw a warning if non-standard columns are found in the data
  if (sum(!names(output) %in% c("statistic", "value", "group", "term")) > 0) {
    warning(paste("Non-standard columns found."))
  }

  # Create the new element
  new_element <- output

  # Filter out statistics
  if (!is.null(statistics)) {
    new_element <- filter(new_element, statistic %in% statistics)
  }

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
