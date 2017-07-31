#' Title
#'
#' \code{tidystats} is a generic function that returns a line of tidy stats.

#' @Dependencies
#' 

#' @examples
#' tidystats.t.test()

tidystats <- function(test, description = "") UseMethod("tidystats")
