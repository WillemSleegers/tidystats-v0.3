#' Report table method for linear regression models
#'
#' Function create a table in order to display results of a regression.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param terms A character vector indicating which terms you want to report in the table.
#' @param term_nrs A numeric vector indicating which terms you want to report in the table.
#' @param statistics A character vector of statistics you want to display in the table.
#' @param include_model Include or exclude model statistics (e.g., R squared).
#' @param term_labels Character vector to change the labels for the terms.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Example: regression term
#' report_table(results, identifier = "regression")
#'
#' @importFrom knitr kable
#'
#' @export

report_table_lm <- function(results, identifier, terms = NULL, term_nrs = NULL, statistics = NULL,
                            include_model = TRUE, term_labels = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]
  res_terms <- filter(res, term != "(Model)")
  res_model <- filter(res, term == "(Model)")

  # Restructure data frames and reorder columns
  res_terms <- res_terms %>%
    spread(statistic, value) %>%
    select(term, b, SE, t, df, p)

  res_model <- res_model %>%
    spread(statistic, value) %>%
    mutate(df = paste(`numerator df`, `denominator df`, sep = ", ")) %>%  # Concatenate the dfs
    select(term, `F`, df, p, `R squared`, `adjusted R squared`)

  # Replace term labels, if provided
  if (!is.null(term_labels)) {
    res_terms$term <- term_labels
  }

  # Combine results, if include_model is TRUE
  if (include_model) {

    res_model <- bind_rows(res_model, data_frame(term = res_terms$term)) %>%
      select(-term)
    res_terms <- bind_rows(data_frame(term = "Model"), res_terms)

    output <- cbind(res_terms, res_model)
  } else {
    output <- res_terms
  }

  return(knitr::kable(output))
}
