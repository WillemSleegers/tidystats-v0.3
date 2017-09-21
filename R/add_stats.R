#' Add statistical output to a tidy stats list
#'
#' \code{add_stats} is a generic function that adds output to a results list. It can take either a statistical model as input or a data frame.

#' @export
add_stats <- function(output, results, identifier = NULL, statistics = NULL, type = NULL,
                      confirmatory = NULL, notes = NULL) UseMethod("add_stats")
