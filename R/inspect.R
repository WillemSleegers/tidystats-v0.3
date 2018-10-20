#' Inspect the output of (a) statistical model(s) via an interactive Shiny app.
#'
#' \code{inspect} is a function to inspect the output of one or more statistical
#' models. The function will open a Shiny widget in the Viewer pane. This allows
#' the user to visually inspect the model output, as well as copy the results in
#' APA format.
#'
#' @param results A tidystats list or the output of a statistical test.
#' @param ... A variable number of identifiers in order to limit the number of
#' models shown when a tidystats list is provided.
#'
#' @export

inspect <- function(results, ...)
  UseMethod("inspect", results)
