#' tidy_stats method for merModLmerTest objects
#'
#' Creates a tidystats data frame for an merModLmerTest object.
#'
#' @param model An merModLmerTest object
#'
#' @examples
#' library(lme4)
#' library(lmerTest)
#'
#' model <- lmer(extra ~ group + (1|ID), data = sleep)
#' tidy_stats.lmerMod(model)
#'
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.merModLmerTest <- function(model) {

  # Get summary statistics
  summary <- lmerTest::summary(model)

  # Extract statistics of random effects
  random <- as_data_frame(summary$varcor) %>%
    mutate(
      term = paste(grp, var1, var2, "(R)", sep = "-"),
      term = gsub("-NA", "", term),
      order = 1:n()
    ) %>%
    rename(
      variance = vcov,
      SD = sdcor
    ) %>%
    gather("statistic", "value", variance, SD) %>%
    arrange(order) %>%
    select(-order, -grp, -var1, -var2)

  # Extract statistics of fixed effects
  fixed <- as_data_frame(summary$coefficients) %>%
    mutate(
      term = paste(rownames(summary$coefficients), "(F)", sep = "-"),
      order = 1:n()
    ) %>%
    rename(
      estimate = `Estimate`,
      SE = `Std. Error`,
      t = `t value`,
      p = `Pr(>|t|)`
    ) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(-order)

  # Combine both
  output <- bind_rows(random, fixed)

  # Not included:
  # - Scaled residuals
  # - Number of obs
  # - Correlation of Fixed Effects

  # Add method
  output$method <- "Linear mixed model"

  return(output)
}
