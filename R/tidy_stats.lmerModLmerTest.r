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
    group = "model"
  )

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
    dplyr::mutate(pair = if_else(is.na(var2), FALSE, TRUE)) %>%
    tidyr::unite(term, grp, starts_with("var"), sep = " - ") %>%
    dplyr::mutate(
      term = stringr::str_replace_all(term, " - ?NA", ""),
      term_nr = 1:n() + max(model_N$term_nr),
      group = "random"
    ) %>%
    tidyr::gather("statistic", "value", vcov, sdcor) %>%
    dplyr::mutate(statistic = case_when(
      statistic == "vcov" & !pair ~ "var",
      statistic == "vcov" & pair ~ "cov",
      statistic == "sdcor" & !pair ~ "sd",
      statistic == "sdcor" & pair ~ "r"
    )) %>%
    dplyr::arrange(term_nr)

  # Extract statistics of fixed effects
  fixed <- tibble::as_data_frame(summary$coefficients) %>%
    dplyr::mutate(
      term = rownames(summary$coefficients),
      term_nr = 1:n() + max(random$term_nr),
      group = "fixed"
    ) %>%
    dplyr::rename(
      estimate = `Estimate`,
      SE = `Std. Error`,
      t = `t value`,
      p = `Pr(>|t|)`
    ) %>%
    tidyr::gather("statistic", "value", -term, -term_nr, -group) %>%
    dplyr::arrange(term_nr)

  # Add fixed effects correlations
  cors <- cov2cor(summary$vcov)
  cors[lower.tri(cors, diag = TRUE)] <- NA
  cors <- cors %>%
    as.matrix() %>% # Coercion to data frame does not work otherwise
    as.data.frame() %>%
    tibble::rownames_to_column("term1") %>%
    tidyr::gather("term2", "value", -term1) %>%
    dplyr::filter(term1 != term2 & !is.na(value)) %>%
    tidyr::unite(col = "term", term1, term2, sep = " - ") %>%
    dplyr::mutate(
      statistic = "r",
      group = "fixed",
      term_nr = 1:n() + max(fixed$term_nr)
    )

  # Combine all parts of the output
  output <- dplyr::bind_rows(model_N, random, fixed, cors)

  # TODO:
  # - REML criterion at convergence
  # - Scaled residuals

  # Add method
  output$method <- "Linear mixed model {lmerTest}"

  # Order variables
  output <- dplyr::select(output, group, term_nr, term, statistic, value,
    method)

  return(output)
}
