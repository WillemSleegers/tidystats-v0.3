#' Create Empty Stats Data Frame
#'
#' This function allows you to create an empty data frame to store the descriptive information and output of statistical models in.

#' @Dependencies
#' tibble, broom

#' @examples
#' new_stats_data_frame()

new_stats_data_frame <- function() {

    return <- tibble::data_frame(
        identifier = NA,
        type = NA,
        term = NA,
        estimate = NA,
        std_error = NA,
        parameter = NA,
        statistic = NA,
        p_value = NA,
        description = NA
    )
}
