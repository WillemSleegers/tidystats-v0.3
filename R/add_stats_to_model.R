#' Add statistical output to a model in a tidy stats list
#'
#' \code{add_stats_to_model} adds output to a model in a tidy results list. Sometimes you have to run additional analyses on the output of a statistical test, so you want to add these results to an existing model in a tidy stats list.
#'
#' @param output output of a statistical test.
#' @param results a tidy stats list.
#' @param identifier a character string identifying the model.
#' @param statistics a vector of statistics to select from the output and add to the model in the tidy stats list.
#'
#' @examples
#' library(magrittr)
#'
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Conduct a regression analysis
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model <- lm(weight ~ group)
#'
#' # Add output to the results list
#' results <- add_stats(model, results, identifier = "M1")
#'
#' # Get confidence intervals of the model
#' model_CIs <- confint(model)
#'
#' # Produce a tidy data frame of the CIs and add it to the results list
#' results <- model_CIs %>%
#'   tidy_stats_confint() %>%
#'   add_stats_to_model(results, identifier = "M1")
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export

add_stats_to_model <- function(output, results, identifier, statistics = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Create the new element
  new_element <- output

  # Check whether the new element contains the necessary columns
  if (!"statistic" %in% names(new_element)) {
    stop("No statistic column found.")
  } else if (!"value" %in% names(new_element)) {
    stop("No value column found.")
  } else if ("term" %in% names(res)) {
    if (length(res$term) > 0) {
      if (!sum(c("term", "term_nr") %in% names(new_element)) > 0) {
        stop("No term information found.")
      }
    }
  }

  # Filter out statistics when only a subset of the statistics are added
  if (!is.null(statistics)) {
    new_element <- dplyr::filter(new_element, statistic %in% statistics)
  }

  # Merge with the model statistics
  if ("term" %in% names(res)) {
    if (length(res$term) > 0) {
      if ("term" %in% names(new_element)) {
        new_element <- dplyr::full_join(res, new_element, by = c("term", "statistic", "value")) %>%
          dplyr::group_by(term) %>%
          dplyr::mutate(
            term_nr = first(term_nr),
            method = first(method)
          )
      } else {
        new_element <- dplyr::full_join(res, new_element, by = c("term_nr", "statistic", "value")) %>%
          dplyr::group_by(term_nr) %>%
          dplyr::mutate(
            term = first(term),
            method = first(method)
          )
      }
      new_element <- new_element %>%
        dplyr::arrange(term_nr) %>%
        dplyr::ungroup()
    }
  } else {
    new_element <- dplyr::full_join(res, new_element, by = c("statistic", "value")) %>%
      dplyr::mutate(method = first(method))
  }

  # If there is a 'confirmatory' column; add the same information to the new rows
  new_element <- mutate(new_element, confirmatory = first(confirmatory))

  # Replace the model statistics
  results[[identifier]] <- new_element

  return(results)
}
