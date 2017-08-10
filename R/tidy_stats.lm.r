#' tidy_stats method for lm objects
#'
#' Creates a tidystats data frame for an lm object.
#'
#' @param model An lm object
#'
#' @examples
#' lm_model <- lm(extra ~ group, data = sleep)
#' tidy_stats.lm(lm_model)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.lm <- function(model) {

  # Get summary statistics
  summary <- summary(model)

  # Extract statistics
  # Not included: Descriptives of residuals, residual standard error, residual degrees of freedom
  output <- as_data_frame(summary$coefficients) %>%
    rename(
      b = Estimate,
      SE = `Std. Error`,
      t = `t value`,
      p = `Pr(>|t|)`
    ) %>%
    mutate(
      term = names(model$coefficients),
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(-order) %>%
    bind_rows(
      data_frame(
        term = "(Model)",
        statistic = c("R squared", "adjusted R squared", "F", "numerator df", "denominator df"),
        value = c(summary$r.squared, summary$adj.r.squared, summary$fstatistic[1],
                summary$fstatistic[2], summary$fstatistic[3])
        )
    )

  # Calculate model fit p value ourselves
  p <- pf(summary$fstatistic[1], summary$fstatistic[2], summary$fstatistic[3], lower.tail = FALSE)

  output <- bind_rows(output, data_frame(term = "(Model)", statistic = "p", value = p))

  # Add method
  output$method <- "Linear regression"

  return(output)
}
