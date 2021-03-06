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
#'
#' @export

report_p_value <- function(p_value) {

  # Check whether the p-value is between 0 and 1
  if (p_value < 0 | p_value > 1) {
    stop("p-value is not between 0 and 1")
  }

  p_value <- case_when(
    p_value >= 0.1 ~ paste("*p* =", stringr::str_replace(
      format(round(p_value, 2), nsmall = 2), "0\\.", ".")),
    p_value < 0.1 & p_value >= .001 ~ paste("*p* =", stringr::str_replace(
      format(round(p_value, 3), nsmall = 3), "0\\.", ".")),
    TRUE ~ paste("*p* < .001")
  )

  return(p_value)
}
