#' Create a tidy stats data frame from a glm object
#'
#' \code{tidy_stats.glm} takes a glm object and converts the object to a tidy
#' stats data frame.
#'
#' @param model Output of \code{glm()}.
#'
#' @examples
#'
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
#' @import dplyr
#' @import tidyr
#'
#' @export

tidy_stats.glm <- function(model) {

  # Get summary statistics
  summary <- summary(model)

  # Extract statistics
  # Not included: Descriptives of residuals, residual standard error, residual
  # degrees of freedom

  # Extract coefficients
  output_coefficients <- summary$coefficients %>%
    tibble::as_data_frame() %>%
    dplyr::rename(
      b = Estimate,
      SE = `Std. Error`
    )

  if (model$family$family %in% c("poisson", "binomial")) {
    output_coefficients <- output_coefficients %>%
      dplyr::rename(
        t = `z value`,
        p = `Pr(>|z|)`
      )
  } else {
    output_coefficients <- output_coefficients %>%
      dplyr::rename(
        t = `t value`,
        p = `Pr(>|t|)`
      )
  }

  output_coefficients <- output_coefficients %>%
    dplyr::mutate(
      df = summary$df[2],
      term = names(model$coefficients),
      term_nr = 1:n(),
      group = "coefficients") %>%
    tidyr::gather("statistic", "value", -term, -term_nr, -group) %>%
    dplyr::arrange(term_nr)

  # Extract model fit
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
