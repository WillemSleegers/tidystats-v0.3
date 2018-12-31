#' Create a tidy stats data frame from an anova object
#'
#' \code{tidy_stats.anova} takes an anova object and tidies the output into a
#' data frame.
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
#' tidy_stats(anova(model_0, model_1, test = "Chisq"))
#'
#' # Logistic regression example
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#'
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' tidy_stats(anova(glm.D93))
#'
#' @export

tidy_stats.anova <- function(model) {

  # Convert model output to a data frame
  output <- tibble::as_data_frame(model)

  # Rename columns
  output <- rename_columns(output)

  # Set term information
  if (sum(stringr::str_detect(attr(model, "heading"), "Model [0-9]")) == 1) {
    output <- output %>%
      dplyr::mutate(
        term = stringr::str_split(attr(model, "heading")[2], "\n")[[1]],
        term_nr = 1:dplyr::n()
      )
  } else {
    output <- output %>%
      dplyr::mutate(
        term = rownames(model),
        term_nr = 1:dplyr::n()
      )
  }

  # Tidy stats
  output <- output %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(term_nr)

  # Add method information
  output <- dplyr::mutate(output, method = "ANOVA Table")

  # If it was a chi-square test on an lm model, indicate this in a notes column
  if (sum(stringr::str_detect(names(model), "Pr\\(>Chi\\)")) == 1) {
    output <- dplyr::mutate(output, notes = "Chi-squared test")
  }

  return(output)
}
