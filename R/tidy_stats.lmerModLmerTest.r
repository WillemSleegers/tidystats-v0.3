#' Create a tidy stats data frame from an lmerModLmerTest object
#'
#' \code{tidy_stats.lmerModLmerTest} takes an lmerModLmerTest object and converts the object to a tidy stats data frame.
#'
#' @param model Output of lmerTest's \code{lmer()}.
#'
#' @examples
#' library(lme4)
#' library(lmerTest)
#'
#' # Conduct a linear mixed model
#' model_lmerTest <- lmer(extra ~ group + (1|ID), data = sleep)
#'
#' tidy_stats(model_lmerTest)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats.lmerModLmerTest <- function(model) {

  # Get summary statistics
  summary <- summary(model)

  # Extract model statistics
  model_N <- data_frame(
    value = summary$devcomp$dims[1],
    statistic = "N",
    term = "(Observations)",
    term_nr = 1,
    group = "model")

  model_N_groups <- as_data_frame(summary$ngrps) %>%
    mutate(
      statistic = "N",
      term = names(summary$ngrps),
      term_nr = 1:n() + 1,
      group = "model"
    )

  model_N <- bind_rows(model_N, model_N_groups)

  # Extract random effects statistics
  random <- as_data_frame(summary$varcor) %>%
    unite(term, grp, starts_with("var"), sep = "-") %>%
    mutate(
      term = gsub("-?NA", "", term),
      term_nr = 1:n() + max(model_N$term_nr),
      group = "random"
    ) %>%
    rename(
      var = vcov,
      SD = sdcor
    ) %>%
    gather("statistic", "value", var, SD) %>%
    arrange(term_nr)

  # Extract statistics of fixed effects
  fixed <- as_data_frame(summary$coefficients) %>%
    mutate(
      term = rownames(summary$coefficients),
      term_nr = 1:n() + max(random$term_nr),
      group = "fixed"
    ) %>%
    rename(
      estimate = `Estimate`,
      SE = `Std. Error`,
      t = `t value`,
      p = `Pr(>|t|)`
    ) %>%
    gather("statistic", "value", -term, -term_nr, -group) %>%
    arrange(term_nr)

  # Combine both
  output <- bind_rows(model_N, random)
  output <- bind_rows(output, fixed)

  # Not included:
  # - REML criterion at convergence
  # - Scaled residuals
  # - Correlation of Fixed Effects

  # Add method
  output$method <- "Linear mixed model {lmerTest}"

  # Order variables
  output <- select(output, group, term_nr, term, statistic, value, method)

  return(output)
}
