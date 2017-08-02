#' tidy_stats method for an lm object
#'
#' Creates a tidy_stats data frame for an lm object.

#' @examples
#' lm_model <- lm(extra ~ group, data = sleep)
#' tidy_stats.lm(lm_model")

#'@import dplyr
#'@importFrom magrittr %>%

#'@export
tidy_stats.lm <- function(model, identifier, type = "other", description = NULL) {

  # Tidy the result to a data.frame
  tidy(model) %>%
    rename(
      p_value = p.value,
      std_error = std.error) %>%
    select(one_of("term", "estimate", "std_error", "statistic", "p_value")) %>%
    mutate(
      identifier = identifier,
      type = type,
      method = "Linear regression"
    ) %>%
    select(identifier, type, method, term, everything()) -> output

  # Add description if provided
  if (!is.null(description)) {
    output %>%
      mutate(
        description = description
      ) -> output
  }

  return(output)
}
