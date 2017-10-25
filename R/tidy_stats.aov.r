#' Create a tidy stats data frame from an aov object
#'
#' \code{tidy_stats.aov} takes an aov object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{aov} without within-subject factors.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.aov <- function(model) {

  # Extract statistics
  output <- as_data_frame(summary(model)[[1]]) %>%
    rename(
      SS = `Sum Sq`,
      MS = `Mean Sq`,
      `F` = `F value`,
      df = Df,
      p = `Pr(>F)`
    ) %>%
    mutate(
      term = c(labels(model$terms), "Residuals"),
      term_nr = 1:n()) %>%
    gather("statistic", "value", -term, -term_nr) %>%
    filter(!is.na(value)) %>%
    arrange(term_nr)

  # Determine the type of ANOVA
  classes <- attr(model$terms, "dataClasses")[-1]
  method <- case_when(
    sum(classes == "numeric") > 0 ~ "ANCOVA",
    sum(classes == "factor") == 1 ~ "One-way ANOVA",
    sum(classes == "factor") == 2 ~ "Factorial ANOVA",
    TRUE ~ "ANOVA"
  )
  output$method <- method

  # Reorder columns
  output <- select(output, term_nr, everything())

  return(output)
}
