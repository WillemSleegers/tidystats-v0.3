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
#' # Conduct statistical tests
#' model <- lm(extra ~ group, data = sleep)
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

  # Filter out statistics
  if (!is.null(statistics)) {
    new_element <- dplyr::filter(new_element, statistic %in% statistics)
  }

  # Merge with the model statistics
  new_element <- dplyr::full_join(res, new_element, by = c("term", "statistic", "value")) %>%
    dplyr::group_by(term) %>%
    dplyr::mutate(term_nr = first(term_nr)) %>%
    dplyr::arrange(term_nr) %>%
    dplyr::ungroup()

  # Replace the model statistics
  results[[identifier]] <- new_element

  return(results)
}
