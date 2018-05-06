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
#' report_p_value(c(0.321, 0.0312, 0.00000098))
#'
#' @export

report_p_value <- function(p_value) {

  p_value <- case_when(
    p_value >= 0.1 ~
      paste("*p* =", str_replace(format(round(p_value, 2), nsmall = 2), "0\\.", ".")),
    p_value < 0.1 & p_value >= .001 ~
      paste("*p* =", str_replace(format(round(p_value, 3), nsmall = 3), "0\\.", ".")),
    TRUE ~ paste("*p* < .001")
  )

  return(p_value)
}
