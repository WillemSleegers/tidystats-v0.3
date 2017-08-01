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
    identifier = NA,
    type = NA,
    method = NA,
    term = NA,
    estimate = NA,
    std_error = NA,
    parameter = NA,
    statistic = NA,
    p_value = NA,
    effect_size = NA,
    description = NA
  )
  output$type <- factor(output$type, levels = c("hypothesis", "manipulation check", "contrast",
                                                "other"))
  return(output)
}
