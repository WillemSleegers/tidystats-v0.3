#' Report method for a correlation.
#'
#' Function to report a correlation in APA style.

#' @examples
#' results <- new_stats_data_frame()
#'
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#' cor_test <- cor.test(x, y)
#'
#' results <- add_stats(results, cor_test, "cor_test1")
#' report_correlation(results, "cor_test1")
#'

#'@import dplyr
#'@importFrom magrittr %>%

#'@export
report_correlation <- function(results, ID, statistic = NULL) {
  # Get the parameters
  res <- filter(results, identifier == ID)

  if (res$method == "Pearson's product-moment correlation") {
    res %>%
      select(estimate, parameter, p_value) %>%
      mutate_at(vars(estimate, parameter), ~ formatC(., digits = 2, format = "f")) %>%
      mutate(p_value = report_p_value(p_value)) -> res
  }

  # Report the test
  if (is.null(statistic)) {
    with(res,
         paste0("*r*(", res$parameter, ") = ", estimate, ", ", p_value)
    )
  } else {
    # Report a specific statistic
    res[statistic]
  }
}
