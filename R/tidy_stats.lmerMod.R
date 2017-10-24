#' tidy_stats method for lmerMod objects
#'
#' Creates a tidystats data frame for an lmerMod object.
#'
#' @param model An lmerMod object
#'
#' @examples
#' library(lme4)
#'
#' lme4_model <- lmer(extra ~ group + (1|ID), data = sleep)
#' tidy_stats.lmerMod(lme4_model)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.lmerMod <- function(model) {

  # Get summary statistics
  summary <- summary(model)

  # Extract model statistics
  model <- as_data_frame(summary$AICtab) %>%
    mutate(
      statistic = names(summary$AICtab),
      group = "model",
    ) %>%
    bind_rows(data_frame(
      value = summary$devcomp$dims[1],
      statistic = "N",
      group = "model"))

  model_terms <- as_data_frame(summary$ngrps) %>%
    mutate(
      statistic = "N",
      term = names(summary$ngrps),
      group = "model"
    )

  model <- bind_rows(model, model_terms)

  # Extract random effects statistics
  as_data_frame(summary$varcor) %>%
    mutate(
      term = paste(grp, var1, var2, sep = "_"),
      term = gsub("-NA", "", term),
      order = 1:n(),
      group = "random effect"
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
      term = rownames(summary$coefficients),
      order = 1:n(),
      group = "fixed effect"
    ) %>%
    rename(
      estimate = `Estimate`,
      SE = `Std. Error`,
      t = `t value`
    ) %>%
    gather("statistic", "value", -term, -order, -notes) %>%
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

  # Order variables
  output <- select(output, term, statistic, value, method, notes)

  return(output)
}
