#' Tidystats generic function
#'
#' \code{tidystats} is a generic function that returns a line of tidy stats.

#' @examples
#' tidystats.t.test()

tidystats <- function(model, identifier, type = "", description = NULL) UseMethod("tidystats")
