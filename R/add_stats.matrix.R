#' add_stats matrix function
#'
#' \code{add_stats.matrix} is a function to add a matrix to a tidystats list.
#' Some statistical functions (e.g., \code{confint}) return a matrix. Normally
#' this would not be enough information for tidystats to figure out how to tidy
#' the results. However, if you run \code{add_stats} and provide a class,
#' tidystats may be able to tidy the output. See 'Details' for a list of
#' supported classes.
#'
#' @param results A tidystats list.
#' @param output A matrix that contains statistical output.
#' @param identifier A character string identifying the model. Automatically
#' created if not provided.
#' @param type A character string indicating the type of test. One of
#' "hypothesis", "manipulation check", "contrast", "descriptives", or "other".
#' Can be abbreviated.
#' @param confirmatory A boolean to indicate whether the statistical test was
#' confirmatory (TRUE) or exploratory (FALSE). Can be NA.
#' @param notes A character string to add additional information. Some
#' statistical tests produce notes information, which will be overwritten if
#' notes are provided.
#' @param class A character string to indicate which function was used to
#' produce the output. See 'Details' for a list of supported functions.
#'
#' @details Supported classes include: \code{confint}
#'
#' @examples
#'
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Example: Confidence intervals on a regression model
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model_lm <- lm(weight ~ group)
#' model_lm_confint <- confint(model_lm)
#'
#' # Add output to the results list
#' results <- add_stats(results, model_lm_confint, identifier = "lm_confint",
#'   class = "confint")
#'
#' @export
add_stats.matrix <- function(results, output, identifier = NULL, type = NULL,
  confirmatory = NULL, notes = NULL, class = NULL) {

  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {
    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(results)+1, width = "1",
        format = "d"))
    } else {
      identifier <- deparse(substitute(output))
    }
  } else {
    if (!is.null(names(results))) {
      if (identifier %in% names(results)) {
        stop("Identifier already exists.")
      }
    }
  }

  # Check if a class was provided
  if (!is.null(class)) {
    class(output) <- append(class(output), class)
    new_element <- tidy_stats(output)
  } else {
    stop("No class provided. Cannot tidy the matrix.")
  }

  # Add the type
  if (!is.null(type)) {
    type <- match.arg(type, choices = c("hypothesis", "manipulation check",
      "contrast", "descriptives", "other"))

    new_element <- dplyr::mutate(new_element, type = dplyr::case_when(
      substr(type, 1, 1) == "h" ~ "hypothesis",
      substr(type, 1, 1) == "m" ~ "manipulation check",
      substr(type, 1, 1) == "c" ~ "contrast",
      substr(type, 1, 1) == "d" ~ "descriptives",
      TRUE ~ NA_character_)
    )
  }

  # Add information whether the analysis was confirmatory or not
  if (!is.null(confirmatory)) {
    new_element <- mutate(new_element, confirmatory = dplyr::case_when(
      confirmatory == TRUE ~ TRUE,
      confirmatory == FALSE ~ FALSE,
      TRUE ~ NA)
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
