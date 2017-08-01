#' Report method for a t-test.
#'
#' Function to report a t-test in APA style.

#' @examples
#' results <- new_stats_data_frame()
#' ttest <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, ttest, "ttest1")
#' report_t_test(results, "ttest1")

#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

#'@export
report_t_test <- function(results, identifier, statistic = NULL) {
  # Get the parameters
  res <- results %>%
    filter(identifier == identifier) %>%
    select(parameter, estimate, p.value, effect_size) %>%
    mutate_at(vars(estimate, parameter, effect_size), ~ formatC(., digits = 2, format = "f")) %>%
    mutate(p.value = report_p_value(p.value))

  # Report the test
  if (is.null(statistic)) {
    with(res,
      paste0("*t*(", res$parameter, ") = ", estimate, ", ", p.value, ", *d* = ",
           effect_size)
    )
  } else {
  # Report a specific statistic
    res[statistic]
  }
}
