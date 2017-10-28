#' tidy_stats method for confint output.
#'
#' Creates a tidystats data frame from the output of confint.
#'
#' @param confint_matrix The output of confint().
#'
#' @examples
#' # Conduct a regression analysis
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model <- lm(weight ~ group)
#'
#' # Get confidence intervals of the model
#' model_CIs <- confint(model)
#'
#' # Produce a tidy data frame of the CIs
#' tidy_model_CIs <- tidy_stats_confint(model_CIs)
#'
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export

tidy_stats_confint <- function(confint_matrix) {

  # Tidy the matrix
  output <- tibble::as_data_frame(confint_matrix) %>%
    dplyr::mutate(
      term = rownames(confint_matrix),
      order = 1:n()) %>%
    tidyr::gather("statistic", "value", -term, -order) %>%
    dplyr::arrange(order) %>%
    dplyr::select(term, statistic, value, -order)

  # Add method
  method = "confint confidence intervals"

  return(output)
}
