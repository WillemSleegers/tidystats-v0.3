#' Report table method for linear regression models
#'
#' Function create a table in order to display results of a regression.
#'
#' @param identifier A character string identifying the model.
#' @param terms A character vector indicating which terms you want to report in the table.
#' @param term_nrs A numeric vector indicating which terms you want to report in the table.
#' @param statistics A character vector of statistics you want to display in the table.
#' @param include_model Include or exclude model statistics (e.g., R squared).
#' @param term_labels A character vector to change the labels for the terms.
#' @param results A tidystats list.
#'
#' @export

report_table_lm <- function(identifier, terms = NULL, term_nrs = NULL,
  statistics = NULL, include_model = TRUE, term_labels = NULL,
  results = getOption("tidystats_list")) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Separate the terms and model statistics
  res_terms <- dplyr::filter(res, term != "(Model)")
  res_model <- dplyr::filter(res, term == "(Model)")

  # Filter out terms, if provided
  if (!is.null(terms)) {
    res_terms <- dplyr::filter(res_terms, term %in% terms)
  } else if (!is.null(term_nrs)) {
    res_terms <- dplyr::filter(res_terms, term_nr %in% term_nrs)
  }

  # Prepare term results
  res_terms <- res_terms %>%
    tidyr::spread(statistic, value) %>%
    dplyr::select(-term_nr, -method)

  # Format p-values
  res_terms$p <- report_p_value(res_terms$p)
  res_terms$p <- stringr::str_replace_all(res_terms$p, "[*p= ]", "")
  res_terms$p <- stringr::str_replace(res_terms$p, "<", "< ")

  # Round statistics
  res_terms <- dplyr::mutate_if(res_terms, is.numeric, round, 2)

  # Check whether confidence intervals are included
  # If so, combine them into a single column called 'CI'
  if (sum(stringr::str_detect(names(res_terms), "[0-9]+ %")) > 0) {
    res_terms <- res_terms %>%
      tidyr::unite(col = CI, matches("[0-9]+ %"), sep = "; ")
  }

  # Reorder columns
  res_terms <- dplyr::select(res_terms, term, b, SE, matches("CI"), t, df, p)

  # Replace term labels, if provided
  if (!is.null(term_labels)) {
    res_terms$term <- term_labels
  }

  # Filter out statistics, if provided
  if (!is.null(statistics)) {
    # Check if the statistics argument only contains valid statistics
    if (sum(!statistics %in% c("b", "SE", "t", "df", "p")) > 0) {
      stop("The statistics argument contains invalid statistics.")
    }

    res_terms <- dplyr::select(res_terms, term, one_of(statistics))
  }

  # Add asterisks to statistic columns to make them cursive
  names(res_terms) <- paste0("*", names(res_terms), "*")

  # Rename 'term' to 'Term'
  res_terms <- dplyr::rename(res_terms, Term = `*term*`)

  # Determine alignment
  align <- dplyr::case_when(
    names(res_terms) == "Term" ~ "l",
    TRUE ~ "r"
  )

  # Create table
  output <- knitr::kable(res_terms, align = align, caption = paste("Regression output of", identifier,
                                                            digits = NULL))

  # Add model results, if not excluded
  if (include_model) {
    footnote_model <- paste("Model:", report_lm(results, identifier, term = "(Model)"))
    output[length(output) + 1] <- ""
    output[length(output) + 1] <- footnote_model
  }

  return(output)
}
