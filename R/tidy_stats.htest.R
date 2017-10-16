#' Create a tidy stats data frame from an htest object
#'
#' \code{tidy_stats.htest} takes an htest object and converts the object to a tidy stats data frame.
#'
#' @param model An htest object
#'
#' @examples
#' tidy_stats.htest(t.test(1:10, y = c(7:20)))
#'
#' @import tibble
#' @importFrom dplyr bind_rows
#'
#' @export
tidy_stats.htest <- function(model) {

  # Extract statistics
  output <- bind_rows(
    data_frame(statistic = names(model$estimate), value = model$estimate),
    data_frame(statistic = names(model$statistic), value = model$statistic),
    if (!is.null(model$parameter)) {
      data_frame(statistic = names(model$parameter), value = model$parameter)
    },
    data_frame(statistic = "p", value = model$p.value),
    if (!is.null(model$conf.int)) {
      data_frame(
        statistic = c("95% CI lower", "95% CI upper"),
        value = c(model$conf.int[1], model$conf.int[2])
      )
    },
    data_frame(statistic = "null value", value = model$null.value)
  )

  # Add the method
  output$method <- model$method

  # Add additional information
  output$notes <- paste(model$alternative, "test")

  return(output)
}




