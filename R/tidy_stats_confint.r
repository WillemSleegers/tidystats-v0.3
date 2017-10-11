#' tidy_stats method for confint output.
#'
#' Creates a tidystats data frame from the output of confint.
#'
#' @param matrix confint matrix.
#'
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats_confint <- function(confint_matrix, method = NULL) {

  # Tidy the matrix
  output <- as.data.frame(confint_matrix) %>%
    mutate(
      term = rownames(confint_matrix),
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(term, statistic, value, -order)

  # Add method
  if (!is.null(method)) {
    output$method = method
  } else {
    method = "confint confidence intervals"
  }

  return(output)
}
