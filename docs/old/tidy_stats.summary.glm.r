#' tidy_stats method for summary.glm objects
#'
#' Creates a tidystats data frame for a summary.glm object.
#'
#' @param model A summary.glm object
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.summary.glm <- function(model) {

  # Get summary statistics
  summary <- summary(model)

  # Extract statistics
  output <- as_data_frame(summary$coefficients) %>%
    rename(
      b = Estimate,
      SE = `Std. Error`,
      z = `z value`,
      p = `Pr(>|z|)`
    ) %>%
    mutate(
      `odds ratio` = exp(coef(model)),
      term = names(model$coefficients),
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(-order) %>%
    bind_rows(
      data_frame(
        term = "(Model)",
        statistic = c("null deviance", "null deviance df", "residual deviance",
                      "residual deviance df", "AIC"),
        value = c(summary$null.deviance, summary$df.null, summary$deviance,
                  summary$df.residual, summary$aic)
      )
    )

  # Add method
  output <- mutate(output, case_when(method =
    model$family$family == "binomial" ~ "Logistic regression",
    model$family$family == "poisson" ~ "Poisson regression"
  ))

  return(output)
}
