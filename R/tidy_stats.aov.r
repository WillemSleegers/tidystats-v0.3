#' tidy_stats method for an aov object
#'
#' Creates a tidystats data frame for an aov object.
#'
#' @param model An aov object
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
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    filter(!is.na(value)) %>%
    arrange(order) %>%
    select(-order)

  # Determine the type of ANOVA
  classes <- attr(model$terms, "dataClasses")[-1]
  method <- case_when(
    sum(classes == "numeric") > 0 ~ "ANCOVA",
    sum(classes == "factor") == 1 ~ "One-way ANOVA",
    sum(classes == "factor") == 2 ~ "Factorial ANOVA",
    TRUE ~ "ANOVA"
  )
  output$method <- method

  return(output)
}
