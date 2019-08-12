#' Create a tidy stats data frame from an lmerMod object
#'
#' \code{tidy_stats.lmerMod} takes an lmerMod object and converts the object to
#' a tidy stats data frame.
#'
#' @param model Output of lme4's \code{lmer()}.
#' @param args Unused.
#'
#' @examples
#' # Check if lme4 package is available
#' if(!requireNamespace("lme4", quietly = TRUE)) {
#' 
#'   message(paste0("Package 'lme4' is needed for this example to work. ",
#'               "Please install it."), .call = FALSE) 
#' } else {
#' 
#'   # Conduct a linear mixed model
#'   model_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'
#'   # Tidy stats
#'   tidy_stats(model_lmer)
#' }
#'
#' @export
tidy_stats.lmerMod <- function(model, args = NULL) {

  # Get summary statistics
  summary <- summary(model)

  # Extract model statistics
  model_N <- tibble::data_frame(
    value = summary$devcomp$dims[1],
    statistic = "N",
    term = "(Observations)",
    term_nr = 1,
    group = "model"
  )

  model_N_groups <- tibble::as_data_frame(summary$ngrps) %>%
    dplyr::mutate(
      statistic = "N",
      term = names(summary$ngrps),
      term_nr = 1:dplyr::n() + 1,
      group = "model"
    )

  model_N <- dplyr::bind_rows(model_N, model_N_groups)

  # Extract random effects statistics
  random <- tibble::as_data_frame(summary$varcor) %>%
    dplyr::mutate(pair = if_else(is.na(var2), FALSE, TRUE)) %>%
    tidyr::unite(term, grp, starts_with("var"), sep = " - ") %>%
    dplyr::mutate(
      term = stringr::str_replace_all(term, " - ?NA", ""),
      term_nr = 1:dplyr::n() + max(model_N$term_nr),
      group = "random"
    ) %>%
    tidyr::gather("statistic", "value", vcov, sdcor) %>%
    dplyr::mutate(statistic = case_when(
      statistic == "vcov" & !pair ~ "var",
      statistic == "vcov" & pair ~ "cov",
      statistic == "sdcor" & !pair ~ "sd",
      statistic == "sdcor" & pair ~ "r")
    ) %>%
    dplyr::arrange(term_nr)

  # Convert fixed effects statistics to a data frame
  fixed <- tibble::as_data_frame(summary$coefficients)

  # Rename columns
  fixed <- rename_columns(fixed)

  # Set group and term information
  fixed <- fixed %>%
    dplyr::mutate(
      term = rownames(summary$coefficients),
      term_nr = 1:dplyr::n() + max(random$term_nr),
      group = "fixed"
    )

  # Tidy stats
  fixed <- fixed %>%
    tidyr::gather("statistic", "value", -term, -term_nr, -group) %>%
    dplyr::arrange(term_nr)

  # Add fixed effects correlations
  cors <- lme4::cov2sdcor(as.matrix(summary$vcov))
  cors[lower.tri(cors, diag = TRUE)] <- NA
  cors <- cors %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term1") %>%
    tidyr::gather("term2", "value", -term1) %>%
    dplyr::filter(term1 != term2 & !is.na(value)) %>%
    tidyr::unite(col = "term", term1, term2, sep = " - ") %>%
    dplyr::mutate(
      statistic = "r",
      group = "fixed",
      term_nr = 1:dplyr::n() + max(fixed$term_nr)
    )

  # Combine all parts of the output
  output <- dplyr::bind_rows(model_N, random, fixed, cors)

  # Not included:
  # - REML criterion at convergence
  # - Scaled residuals

  # Add method
  output <- dplyr::mutate(output, method = "Linear mixed model {lme4}")

  # Order variables
  output <- dplyr::select(output, group, term_nr, term, statistic, value,
    method)

  return(output)
}
