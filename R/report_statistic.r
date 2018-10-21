#' Report a single statistic
#'
#' A helper function to report single statistics in APA style.
#'
#' @param statistic A character string indicating the kind of statistic (e.g.,
#' p, t, df).
#' @param value The value of the statistic to be reported.
#'
#' @examples
#' report_statistic("p", 0.0001)
#' report_statistic("t", 4.2325)
#' report_statistic("cor", 0.213)
#'
#' @export

report_statistic <- function(statistic, value) {

  # Check whether both a statistic and value have been provided
  if (is.null(statistic)) {
    stop("Please provide the label of the statistic (e.g., p, t, df)")
  }
  if (is.null(value)) {
    stop("Please provide the value of the statistic.")
  }

  # Check what kind of statistic is requested and convert the value accordingly
  if (statistic == "p" & value < 0.001) {
    output <- "< .001"
  } else if (statistic %in% c("p", "r", "cor", "rho", "tau")) {
    output <- format(value, digits = 2, nsmall = 2)
    output <- stringr::str_replace(output, "0\\.", ".")
  } else if ((statistic == "df" & value %% 1 == 0) |
      statistic %in% c("n", "N")) {
    output <- format(value, digits = 2, nsmall = 0)
  } else if (statistic %in% c("V", "W")) {
    if (value %% 1 == 0) {
      output <- format(value)
    } else{
      output <- format(value, digits = 2, nsmall = 2)
    }
  } else {
    output <- format(value, digits = 2, nsmall = 2)
  }

  return(output)
}
