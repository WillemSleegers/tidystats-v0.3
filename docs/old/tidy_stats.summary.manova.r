#' tidy_stats method for manova objects
#'
#' Creates a tidystats data frame for a manova object.
#'
#' @param model A manova object
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.summary.manova <- function(model) {

  # Extract statistics
  output <- as_data_frame(model$stats) %>%
    rename(
      `df model` = Df,
      `df numerator` = `num Df`,
      `df denominator` = `den Df`,
      p = `Pr(>F)`
    ) %>%
    mutate(
      term = model$row.names,
      order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    arrange(order) %>%
    select(-order) %>%
    filter(!is.na(value))

  # Add method
  output$method <- "MANOVA"

  return(output)
}
