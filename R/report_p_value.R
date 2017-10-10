#' Report p-value function
#'
#' A helper function to report p values in APA style.

#' @examples
#' report_p_value(.45)
#' report_p_value(.002)
#' report_p_value(.00000122)

#'@export
report_p_value <- function(p_value) {
  if (p_value >= .001) {
    paste("*p* =", gsub(
                     pattern = "0\\.",
                     replacement = ".",
                     x = format(p_value, digits = 2))
    )
  } else {
    paste("*p* < .001")
  }
}
