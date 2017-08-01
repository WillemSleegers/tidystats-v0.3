#' Create Empty Descriptives Data Frame
#'
#' This function allows you to create an empty data frame to store descriptives in.

#' @examples
#' new_descriptives_data_frame()
#'
#'@import tibble

#'@export
new_descriptives_data_frame <- function() {

  return <- tibble::data_frame(
    identifier = character(),
    group = character(),
    outcome = character(),
    mean = double(),
    sd = double(),
    n = integer()
  )
}
