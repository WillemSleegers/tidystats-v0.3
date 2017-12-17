#' Report p-value function
#'
#' A helper function to report p values in APA style.
#'
#' @param p_value One or more p-values.
#'
#' @examples
#' report_p_value(0.532)
#' report_p_value(0.0432)
#' report_p_value(0.0000121)
#' report_p_value(0.321, 0.0312, 0.00000098)
#'
#' @export

report_p_value <- function(p_value) {

  p_value <- if_else(p_value >= .001,
                     paste("*p* =", gsub(pattern = "0\\.", replacement = ".",
                                         x = round(p_value, digits = 3))),
                     paste("*p* < .001"))

  return(p_value)
}
