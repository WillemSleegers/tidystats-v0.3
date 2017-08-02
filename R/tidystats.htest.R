#' Tidystats method for an htest object
#'
#' Creates a tidystats data.frame for an htest object.

#' @examples
#' ttest <- t.test(1:10, y = c(7:20))
#' tidystats.htest("ttest_x_y", "Hypothesis", ttest, "A t-test for x and y")
#'
#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

#'@export
tidystats.htest <- function(model, identifier, type = "other", description = NULL) {

  # Tidy the result to a data.frame
    # Set the variables to select
    if (!is.null(model$parameter)) {
      vars <- c("estimate", "statistic", "p_value", "parameter", "method")
    } else {
      vars <- c("estimate", "statistic", "p_value", "method")
    }

    # Create the variables
    tidy(model) %>%
      rename(p_value = p.value) %>%
      select(one_of(vars)) %>%
      mutate(
        identifier = identifier,
        type = type
      ) %>%
      select(identifier, type, method, everything()) -> output

  # Add description if provided
  if (!is.null(description)) {
    output %>%
      mutate(
        description = description
      ) -> output
  }

  # Add Cohen's d
  if (grepl("t-test", model$method)) {
    output$cohens_d <- 2 * model$statistic / sqrt(model$parameter)
  }

  return(output)
}
