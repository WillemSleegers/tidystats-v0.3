#' Tidystats generic function
#'
#' \code{tidystats} is a generic function that returns a line of tidy stats.

#' @Dependencies
#' 

#' @examples
#' tidystats.t.test()

tidystats <- function(identifer, model, description = NULL) UseMethod("tidystats")
