#' Report generic function
#'
#' \code{report} is a generic function that reports a test in APA style.

# @examples
#

#'@export
report <- function(results, identifier, term = NULL, statistic = NULL) UseMethod("report")
