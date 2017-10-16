#' Create a tidy stats data frame from an anova object
#'
#' \code{tidy_stats.anova} takes an anova object and converts the object to a tidy stats data frame.
#'
#' @param model An anova object
#'
#' @examples
#' lm1 <- lm(extra ~ 1, data = sleep)
#' tidy_stats.anova(lm1)
#'
#' lm2 <- lm(extra ~ group, data = sleep)
#' tidy_stats.anova(lm1, lm2)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats.anova <- function(model) {

  # Check whether it's a one model ANOVA or multiple
  if (!grepl("Model", attr(model, "heading")[2])) {
    output <- as_data_frame(model) %>%
      mutate(
        term = row.names(model),
        order = 1:n()
      ) %>%
      rename(
        df = Df,
        SS = `Sum Sq`,
        MS = `Mean Sq`,
        `F` = `F value`
      ) %>%
      rename_if(grepl("Pr\\(", names(.)), funs(sprintf("p", .))) %>%
      gather("statistic", "value", -term, -order) %>%
      filter(!is.na(value)) %>%
      mutate(method = "Single model ANOVA") %>%
      arrange(order) %>%
      select(-order)
  } else {
    output <- as_data_frame(model) %>%
      mutate(order = 1:n()) %>%
      rename(df = Df)

    if ("AIC" %in% names(output)) {
      output <- output %>%
        mutate(
          term = row.names(model)
        ) %>%
        rename(
          `log likelihood` = logLik,
          `chi-squared` = Chisq,
          `df chi-squared` = `Chi Df`,
          p = `Pr(>Chisq)`
        )
    } else {
      output <- output %>%
        mutate(term = strsplit(attr(model, "heading")[2], split = "\n")[[1]]) %>%
        rename(
          `df residual` = `Res.Df`,
          SS = `Sum of Sq`
        ) %>%
        rename_if(grepl("Pr\\(", names(.)), funs(sprintf("p", .)))
    }

    output <- output %>%
      gather("statistic", "value", -term, -order) %>%
      filter(!is.na(value)) %>%
      mutate(method = "Multiple model ANOVA") %>%
      arrange(order) %>%
      select(-order)
  }

  # If it was a chi-square test on an lm model, indicate this in a notes column
  if (grepl("Pr\\(>Chi\\)", names(model)[5])) {
    output$notes <- "Chi-square test"
  }

  return(output)
}
