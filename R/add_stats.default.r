#' add_stats default function
#'
#' \code{add_stats.default} is the default add_stats function, which takes the output of a statistical test, tidies the output, and adds it to a results list.
#'
#' @param output output of a statistical test.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model. Automatically created if not provided.
#' @param statistics a vector of statistics to select from the output and add to the tidy stats list.
#' @param type a character string indicating the type of test. One of "hypothesis", "manipulation check", "contrast", "descriptives", or "other". Can be abbreviated.
#' @param confirmatory a boolean to indicate whether the statistical test was confirmatory (TRUE) or exploratory (FALSE). Can be NA.
#' @param notes a character string to add additional information. Some statistical tests produce notes information, which will be overwritten if notes are provided.
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
      confirmatory == TRUE ~ TRUE,
      confirmatory == FALSE ~ FALSE,
      TRUE ~ NA
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
