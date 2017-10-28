#' Report p-value function
#'
#' A helper function to report p values in APA style.
#'
#' @param p_value A p-value.
#'
#' @examples
#' report_p_value(0.532)
#' report_p_value(0.0432)
#' report_p_value(0.0000121)
#'
#' @export

report_p_value <- function(p_value) {
  if (p_value >= .001) {
    paste("*p* =", gsub(pattern = "0\\.", replacement = ".", x = format(p_value, digits = 2)))
  } else {
    paste("*p* < .001")
  }
}
