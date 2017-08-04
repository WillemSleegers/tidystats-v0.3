#' tidy_stats method for an htest object
#'
#' Creates a tidystats data frame for an htest object.

#' @examples
#' tidy_stats.htest(t.test(1:10, y = c(7:20)))

#'@import tibble

#'@export
tidy_stats.htest <- function(model) {
  # Create tidy stats data frame
  output <- data_frame(
    method = model$method
  )

  # Add estimate
  if (grepl("Two Sample", model$method)) {
    output$estimate <- model$estimate[[1]]-model$estimate[[2]]
  } else {
    output$estimate <- model$estimate[[1]]
  }

  # Add statistic
  output$statistic <- model$statistic

  # Add degrees of freedom, if possible
  if (!is.null(model$parameter)) {
    output$df <- model$parameter
  }

  # Add additional statistics
  output$p_value <- model$p.value

  return(output)
}
