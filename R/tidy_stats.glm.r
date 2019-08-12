#' Create a tidy stats data frame from a glm object
#'
#' \code{tidy_stats.glm} takes a glm object and converts the object to a tidy
#' stats data frame.
#'
#' @param model Output of \code{glm()}.
#' @param args Unused.
#'
#' @examples
#' # Get data
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#'
#' # Run model
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' # Tidy stats
#' tidy_stats(glm.D93)
#'
#' @export

tidy_stats.glm <- function(model, args = NULL) {

  # Get summary statistics
  summary <- summary(model)

  # Convert model coefficients output to a data frame
  output_coefficients <- summary$coefficients %>%
    tibble::as_data_frame()
  # Not included: Descriptives of residuals, residual standard error, residual
  # degrees of freedom

  # Rename columns
  output_coefficients <- rename_columns(output_coefficients)

  # Set group and term information, and add degrees of freedom
  output_coefficients <- output_coefficients %>%
    dplyr::mutate(
      df = summary$df[2],
      term = names(model$coefficients),
      term_nr = 1:dplyr::n(),
      group = "coefficients")

  # Tidy stats
  output_coefficients <- output_coefficients %>%
    tidyr::gather("statistic", "value", -term, -term_nr, -group) %>%
    dplyr::arrange(term_nr)

  # Extract model fit statistics and immediately tidy the statistics
  output_model <- tibble::data_frame(
    group = "model",
    statistic = c("dispersion", "null deviance", "null deviance df",
      "residual deviance", "residual deviance df", "AIC",
      "Fisher Scoring iterations"),
    value = c(summary$dispersion, summary$null.deviance, summary$df.null,
      summary$deviance, summary$df.residual, summary$aic, summary$iter)
  )

  # Combine coefficients and model output
  output <- bind_rows(output_coefficients, output_model)

  # Add method
  output <- output %>%
    mutate(
      method = "Generalized linear model",
      notes = paste0("family: ", model$family$family, "; link function: ",
        model$family$link)
    )

  # Reorder columns
  output <- dplyr::select(output, group, term_nr, term, everything())

  return(output)
}
