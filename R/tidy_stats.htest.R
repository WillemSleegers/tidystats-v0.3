#' Create a tidy stats data frame from an htest object
#'
#' \code{tidy_stats.htest} takes an htest object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{t.test()}.
#'
#' @examples
#' # Conduct a t-test
#' model_t_test <- t.test(extra ~ group, data = sleep)
#' tidy_stats(model_t_test)
#'
#' @import tibble
#' @import dplyr
#'
#' @export
tidy_stats.htest <- function(model) {

  # Extract statistics
  output <- dplyr::bind_rows(
    tibble::data_frame(statistic = names(model$estimate), value = model$estimate),
    tibble::data_frame(statistic = names(model$statistic), value = model$statistic),
    if (!is.null(model$parameter)) {
      tibble::data_frame(statistic = names(model$parameter), value = model$parameter)
    },
    tibble::data_frame(statistic = "p", value = model$p.value),
    if (!is.null(model$conf.int)) {
      tibble::data_frame(
        statistic = c("95% CI lower", "95% CI upper"),
        value = c(model$conf.int[1], model$conf.int[2])
      )
    },
    tibble::data_frame(statistic = "null value", value = model$null.value)
  )

  # Add the method (use trimws to remove the leading space from a Two Sample t-test)
  output$method <- trimws(model$method)

  # Add additional information
  output$notes <- paste(model$alternative, "test")

  return(output)
}




