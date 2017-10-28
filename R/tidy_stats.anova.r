#' Create a tidy stats data frame from an anova object
#'
#' \code{tidy_stats.anova} takes an anova object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{anova()}.
#'
#' @examples
#' # Regression example
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model_0 <- lm(weight ~ 1)
#' model_1 <- lm(weight ~ group)
#'
#' tidy_stats(anova(model_0))
#' tidy_stats(anova(model_1))
#' tidy_stats(anova(model_0, model_1))
#'
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats.anova <- function(model) {

  # Check whether it's a one model ANOVA or multiple
  if (!grepl("Model", attr(model, "heading")[2])) {
    output <- tibble::as_data_frame(model) %>%
      dplyr::mutate(
        term = row.names(model),
        order = 1:n()
      ) %>%
      dplyr::rename(
        df = Df,
        SS = `Sum Sq`,
        MS = `Mean Sq`,
        `F` = `F value`
      ) %>%
      dplyr::rename_if(grepl("Pr\\(", names(.)), funs(sprintf("p", .))) %>%
      tidyr::gather("statistic", "value", -term, -order) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(method = "Single model ANOVA") %>%
      dplyr::arrange(order) %>%
      dplyr::select(-order)
  } else {
    output <- tibble::as_data_frame(model) %>%
      dplyr::mutate(order = 1:n()) %>%
      dplyr::rename(df = Df)

    if ("AIC" %in% names(output)) {
      output <- output %>%
        dplyr:: mutate(
          term = row.names(model)
        ) %>%
        dplyr::rename(
          `log likelihood` = logLik,
          `chi-squared` = Chisq,
          `df chi-squared` = `Chi Df`,
          p = `Pr(>Chisq)`
        )
    } else {
      output <- output %>%
        dplyr::mutate(term = strsplit(attr(model, "heading")[2], split = "\n")[[1]]) %>%
        dplyr::rename(
          `df residual` = `Res.Df`,
          SS = `Sum of Sq`
        ) %>%
        dplyr::rename_if(grepl("Pr\\(", names(.)), funs(sprintf("p", .)))
    }

    output <- output %>%
      tidyr::gather("statistic", "value", -term, -order) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(method = "Multiple model ANOVA") %>%
      dplyr::arrange(order) %>%
      dplyr::select(-order)
  }

  # If it was a chi-square test on an lm model, indicate this in a notes column
  if (grepl("Pr\\(>Chi\\)", names(model)[5])) {
    output$notes <- "Chi-square test"
  }

  return(output)
}
