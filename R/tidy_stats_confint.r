#' tidy_stats method for confint output.
#'
#' Creates a tidystats data frame from the output of confint.
#'
#' @param confint_matrix The output of confint().
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats_confint <- function(confint_matrix) {

  # Tidy the matrix
  output <- as.data.frame(confint_matrix) %>%
    mutate(
      term = rownames(confint_matrix),
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(term, statistic, value, -order)

  # Add method
  method = "confint confidence intervals"

  return(output)
}
