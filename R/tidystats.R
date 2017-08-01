#' Tidystats generic function
#'
#' \code{tidystats} is a generic function that returns a line of tidy stats.

# @examples
#

#'@export
tidystats <- function(model, identifier, type = "other", description = NULL) UseMethod("tidystats")
