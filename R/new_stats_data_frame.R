#' Create Empty Stats Data Frame
#'
#' This function allows you to create an empty data frame to store the output of statistical models in.

#' @examples
#' new_stats_data_frame()
#'
#'@import tibble

#'@export
new_stats_data_frame <- function() {

  output <- tibble::data_frame(
    identifier = character(),
    type = factor(),
    method = character(),
    term = character(),
    estimate = double(),
    std_error = double(),
    parameter = double(),
    statistic = double(),
    p_value = double(),
    effect_size = double(),
    description = character()
  )
  output$type <- factor(output$type, levels = c("hypothesis", "manipulation check", "contrast",
                                                "other"))
  return(output)
}
