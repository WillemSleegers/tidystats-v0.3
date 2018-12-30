#' tidy_stats method for confint output.
#'
#' Creates a tidystats data frame from the output of confint.
#'
#' @param model The output of confint().
#'
#' @details This method should not be called directly.
#'
#' @export

tidy_stats.confint <- function(model) {
  output <- as.data.frame(model) %>% # as_data_frame() throws an error
    dplyr::mutate(
      term = rownames(model),
      order = 1:n()
      ) %>%
    tidyr::gather("statistic", "value", -term, -order) %>%
    dplyr::arrange(order) %>%
    dplyr::select(term, statistic, value, -order) %>%
    dplyr::mutate(
      statistic = stringr::str_replace(statistic, " ", ""),
      statistic = paste(statistic, "CI")
      ) %>%
    tibble::as_data_frame()

  return(output)
}
