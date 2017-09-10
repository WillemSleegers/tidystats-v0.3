#' confint, with class
#'
#' Helper function that adds a class to the output of confint.
#'
#' @importFrom stats confint
#'
#' @export
confint <- function(object, parm, level = 0.95, ...) {

  output <- stats::confint(object, parm, level = 0.95, ...)

  class(output) <- c("confint","matrix")

  return(output)
}