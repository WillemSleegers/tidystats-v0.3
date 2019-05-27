#' Rename statistics columns
#'
#' Renames the statistics columns.
#'
#' @param x The output of a statistical model, converted to a data frame.
#'
#' @details This function matches each column name with an entry in a list of
#' statistical names and renames the column if necessary. The goal is to create
#' a naming scheme that makes statistical labels as consistent as possible.
#'
#' @importFrom rlang UQS
#'
#' @export
rename_columns <- function(x) {
  # List of statistical labels
  renamers <- c(
    "AIC" = "AIC",
    "BIC" = "BIC",
    "deviance" = "deviance",
    "Deviance" = "deviance",
    "Resid. Dev" = "residual deviance",
    "logLik" = "log-likelihood",
    "Df" = "df",
    "Chi.Df" = "df",
    "Chi Df" = "chi-squared df",
    "Sum Sq" = "SS",
    "Sum.Sq" = "SS",
    "Sum of Sq" = "SS",
    "Mean Sq" = "MS",
    "Mean.Sq" = "MS",
    "F" = "F",
    "F value" = "F",
    "F.value" = "F",
    "Pr(>F)" = "p",
    "num Df" = "numerator df",
    "den Df" = "denominator df",
    "Res.Df" = "denominator df",
    "Resid. Df" = "denominator df",
    "RSS" = "RSS",
    "Chisq" = "chi-squared",
    "Chi.sq" = "chi-squared",
    "LR.Chisq" = "LR chi-squared",
    "LR Chisq" = "LR chi-squared",
    "P(>|Chi|)" = "p",
    "Pr(>Chi)" = "p",
    "Pr(>Chisq)" = "p",
    "Pr..Chisq." = "p",
    "Pr..Chi." = "p",
    "Pr(>|z|)" = "p",
    "Pr(>|t|)" = "p",
    "Pr..F." = "p",
    "p.value" = "p",
    "edf" = "EDF",
    "Ref.df" = "df",
    "Estimate" = "b",
    "Std. Error" = "SE",
    "z value" = "z",
    "t value" = "t",

    "raw_alpha" = "raw alpha",
    "std.alpha" = "standardized alpha",
    "average_r" = "average r",
    "median_r" = "median r",
    "G6(smc)" = "G6",
    "S/N" = "signal/noise ratio",
    "ase" = "alpha SE",

    "mean" = "M",
    "sd" = "SD"
  )

  # Rename columns
  colnames(x) <- dplyr::recode(colnames(x), UQS(renamers))

  return(x)
}
