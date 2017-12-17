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
#' @import knitr
#' @import kableExtra
#'
#' @export

report_table_lm <- function(results, identifier, terms = NULL, term_nrs = NULL, statistics = NULL,
                            include_model = TRUE, term_labels = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Separate the terms and model statistics
  res_terms <- filter(res, term != "(Model)")
  res_model <- filter(res, term == "(Model)")

  # Filter out terms, if provided
  if (!is.null(terms)) {
    res_terms <- filter(res_terms, term %in% terms)
  } else if (!is.null(term_nrs)) {
    res_terms <- filter(res_terms, term_nr %in% term_nrs)
  }

  # Prepare term results
  res_terms <- res_terms %>%
    spread(statistic, value) %>%
    select(-term_nr, -method) %>%
    rename(Term = term) %>%
    select(Term, b, SE, t, df, p)

  # Replace term labels, if provided
  if (!is.null(term_labels)) {
    res_terms$Term <- term_labels
  }

  # Filter out statistics, if provided
  if (!is.null(statistics)) {
    # Check if the statistics argument only contains valid statistics
    if (sum(!statistics %in% c("b", "SE", "t", "df", "p")) > 0) {
      stop("The statistics argument contains invalid statistics.")
    }

    res_terms <- select(res_terms, Term, one_of(statistics))
  }

  # Format p-values, if requested
  if ("p" %in% names(res_terms)) {
    res_terms$p <- report_p_value(res_terms$p)
  }

  # Create table
  format <- print(opts_knit$get("rmarkdown.pandoc.to"))
  output <- kable(res_terms, format = format, escape = FALSE) %>%
    kable_styling(full_width = FALSE, position = "left")

  # Prepare model results, if not excluded
  if (include_model) {
    footnote_model <- paste("Model:", report_lm(results, identifier, term = "(Model)"))
    output <- kableExtra::add_footnote(output, footnote_model, notation = "number")
  }

  return(output)
}
