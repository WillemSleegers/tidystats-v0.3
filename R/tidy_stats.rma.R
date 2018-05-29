#' Create a tidy stats data frame from an rma object from the metafor package
#'
#' \code{tidy_stats.rma} takes an rma object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{metafor::rma()}.
#'
#' @examples
#' library(metafor)
#'
#' # Calculate log risk ratios and corresponding sampling variances
#' dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
#'
#' # Perform a random-effects model
#' model_rma <- rma(yi, vi, data=dat, method="REML")
#'
#' # Tidy the model output
#' tidy_stats(model_rma)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats.rma <- function(model) {

  # Heterogeneity statistics
  heterogeneity <- tibble::data_frame(
    k = model$k,
    `tau^2` = model$tau2,
    `tau^2 SE` = model$se.tau2,
    tau = sqrt(model$tau2),
    `I^2` = model$I2,
    `H^2` = model$H2
  )

  if (!is.null(model$R2)) {
    heterogeneity$`R^2` = model$R2
  }

  heterogeneity$QE = model$QE
  heterogeneity$df = model$k-model$p
  heterogeneity$p = model$QEp

  heterogeneity <- gather(heterogeneity, "statistic", "value")

  heterogeneity$term <- "(Heterogeneity)"
  heterogeneity$term_nr <- 1

  output <- heterogeneity

  # Moderator statistics
  if (model$p > 1 && !is.na(model$QM)) {

    moderator <- tibble::data_frame(`F` = model$QM)

    if (is.na(model$dfs)) {
      moderator$df <- model$m
    } else {
      moderator$numerator_df <- model$m
      moderator$denominator_df <- model$dfs
    }

    moderator$p = model$QMp

    moderator <- gather(moderator, "statistic", "value")

    moderator$term <- "(Moderators)"
    moderator$term_nr <- 2

    output <- bind_rows(output, moderator)
  }

  terms <- length(unique(output$term))

  # Coefficient statistics
  model_results <- tibble::data_frame(
    term = names(model$beta[,1]),
    term_nr = (1 + terms):(terms + length(model$beta)),
    b = model$beta[,1],
    SE = model$se,
    z = model$zval,
    p = model$pval,
    lower = model$ci.lb,
    upper = model$ci.ub
  )

  # Get confidence interval level
  CI_level <- model$call$level
  if (is.null(CI_level)) {
    CI_level <- 95
  }

  names(model_results) <- str_replace(names(model_results), "lower",
                                      paste0((100-CI_level)/2, "% CI"))
  names(model_results) <- str_replace(names(model_results), "upper",
                                      paste0(100-(100-CI_level)/2, "% CI"))

  if (is.element(model$test, c("knha","adhoc","t"))) {
    model_results <- dplyr::rename(model_results, t = z)
  }

  model_results %<>%
    gather("statistic", "value", -term, -term_nr) %>%
    arrange(term_nr)

  # Combine statistics
  output <- bind_rows(output, model_results)

  # Re-order columns
  output <- select(output, term, term_nr, statistic, value)

  # Add method information
  if (model$method == "FE") {
    if (model$int.only) {
      output$method <- "Fixed-Effects Model {metafor}"
    } else {
      output$method <- "Fixed-Effects with Moderators Model {metafor}"
    }
  } else {
    if (model$int.only) {
      output$method <- "Random-Effects Model {metafor}"
    } else {
      output$method <- "Mixed-Effects Model {metafor}"
    }
  }

  # Add notes
  output$notes <- paste(model$method, "tau^2 estimator")

  return(output)
}
