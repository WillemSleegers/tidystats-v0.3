#' tidy_stats method for confint output.
#'
#' Creates a tidystats data frame from the output of confint.
#'
#' @param confint_matrix The output of confint().
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
