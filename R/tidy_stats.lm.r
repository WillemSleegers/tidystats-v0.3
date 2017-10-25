#' Create a tidy stats data frame from an lm object
#'
#' \code{tidy_stats.lm} takes an lm object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{lm()}.
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
      df = summary$df[2],
      term = names(model$coefficients),
      term_nr = 1:n()) %>%
    gather("statistic", "value", -term, -term_nr) %>%
    arrange(term_nr)

  output <- bind_rows(output,
    data_frame(
      term = "(Model)",
      term_nr = max(output$term_nr) + 1,
        statistic = c("R squared", "adjusted R squared", "F", "numerator df", "denominator df"),
      value = c(summary$r.squared, summary$adj.r.squared, summary$fstatistic[1],
                summary$fstatistic[2], summary$fstatistic[3])
    )
  )


  # Calculate model fit p value ourselves
  p <- pf(summary$fstatistic[1], summary$fstatistic[2], summary$fstatistic[3], lower.tail = FALSE)

  output <- bind_rows(output, data_frame(term = "(Model)", term_nr = max(output$term_nr),
                                         statistic = "p", value = p))

  # Add method
  output$method <- "Linear regression"

  # Reorder columns
  output <- select(output, term_nr, everything())

  return(output)
}
