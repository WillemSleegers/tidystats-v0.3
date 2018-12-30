#' Add statistical output to a model in a tidy stats list
#'
#' \code{add_stats_to_model} adds output to a model in a tidy results list.
#' Sometimes you have to run additional analyses on the output of a statistical
#' test, so you want to add these results to an existing model in a tidy stats
#' list.
#'
#' @param results A tidystats list.
#' @param output Output of a statistical test.
#' @param identifier A character string identifying the model.
#' @param class A character string to indicate which function was used to
#' produce the output. See 'Details' for a list of supported functions.
#'
#' @details
#' Supported classes are:
#' - \code{confint}
#'
#' @examples
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
#' results <- add_stats(results, model, identifier = "M1")
#'
#' # Get confidence intervals of the model
#' model_CIs <- confint(model)
#'
#' # Add it to the results list
#' results <- add_stats_to_model(results, model_CIs, identifier = "M1",
#'   class = "confint")
#'
#' @export
add_stats_to_model <- function(results, output, identifier, class = NULL) {

  # Check whether the identifier exists, otherwise extract it
  if (!identifier %in% names(results)) {
    stop("Identifier not found.")
  } else {
    res <- results[[identifier]]
  }

  # Check if a class was provided
  if (!is.null(class)) {
    class(output) <- append(class(output), class)
    new_element <- tidy_stats(output)
  } else {
    # Check whether the new element contains the necessary columns
    if (!"statistic" %in% names(output)) {
      stop("No statistic column found.")
    } else if (!"value" %in% names((output))) {
      stop("No value column found.")
    } else if ("term" %in% names(res)) {
      if (length(res$term) > 0) {
        if (!sum(c("term", "term_nr") %in% names((output))) > 0) {
          stop("No term information found.")
        }
      }
    }

    # Create the new element
    new_element <- output
  }

  # Merge with the model statistics
  if ("term" %in% names(res)) {
    if (length(res$term) > 0) {
      if ("term" %in% names(new_element)) {
        new_element <- dplyr::full_join(res, new_element, by = c("term",
          "statistic", "value")) %>%
          dplyr::group_by(term) %>%
          dplyr::mutate(
            group = first(group),
            term_nr = first(term_nr),
            method = first(method)
          )
      } else {
        new_element <- dplyr::full_join(res, new_element, by = c("term_nr",
          "statistic", "value")) %>%
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
    new_element <- res %>%
      dplyr::full_join(new_element, by = c("statistic", "value")) %>%
      dplyr::mutate(method = first(method))
  }

  # If there is a 'confirmatory' column; add the same information to the new
  # rows
  if (has_name(new_element, "confirmatory")) {
    new_element <- mutate(new_element, confirmatory = first(confirmatory))
  }

  # If there is a 'notes' column; add the same information to the new rows
  if (has_name(new_element, "notes")) {
    new_element <- mutate(new_element, notes = first(notes))
  }

  # Replace the model statistics
  results[[identifier]] <- new_element

  return(results)
}
