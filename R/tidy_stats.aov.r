#' Create a tidy stats data frame from an aov object
#'
#' \code{tidy_stats.aov} takes an aov object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{aov} without within-subject factors.
#'
#' @examples
#' # Conduct an ANOVA
#' model_aov <- aov(yield ~ block + N * P * K, npk)
#' tidy_stats(model_aov)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats.aov <- function(model) {

  # Extract statistics
  output <- tibble::as_data_frame(summary(model)[[1]]) %>%
    dplyr::rename(
      SS = `Sum Sq`,
      MS = `Mean Sq`,
      `F` = `F value`,
      df = Df,
      p = `Pr(>F)`
    ) %>%
    dplyr::mutate(
      term = row.names(data.frame(summary(model)[[1]])),
      term_nr = 1:n()) %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(term_nr)

  # Remove spaces from term
  output$term <- gsub(" ", "", output$term)

  # Determine the type of ANOVA
  classes <- attr(model$terms, "dataClasses")[-1]
  method <- dplyr::case_when(
    sum(classes == "numeric") > 0 ~ "ANCOVA",
    sum(classes == "factor" | classes == "character") == 1 ~ "One-way ANOVA",
    sum(classes == "factor" | classes == "character") == 2 ~ "Factorial ANOVA",
    TRUE ~ "ANOVA"
  )
  output$method <- method

  # Reorder columns
  output <- dplyr::select(output, term_nr, everything())

  return(output)
}
