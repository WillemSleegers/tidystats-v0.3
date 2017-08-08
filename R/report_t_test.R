#' Report method for a t-test.
#'
#' Function to report a t-test in APA style.

#' @examples
#' results <- new_stats_data_frame()
#' ttest <- t.test(1:10, y = c(7:20))
#' results <- add_stats(results, ttest, "ttest1")
#' report_t_test(results, "ttest1")
#'
#' @import dplyr
#' @importFrom magrittr %>%

#'@export
report_t_test <- function(results, identifier, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check if only a single statistic is asked, otherwise produce a full line of APA results
  if (!is.null(statistic)) {
    output <- pull(res[statistic])

    if (statistic == "df_error" & !grepl("Welch", res$method)) {
      output <- formatC(output, digits = 0, format = "d")
    } else {
      output <- formatC(output, digits = 2, format = "f")
    }

  } else {
    res <- res %>%
      mutate_at(vars(statistic), ~ formatC(., digits = 2, format = "f")) %>%
      mutate(df_error = if_else(grepl("Welch", method),
                          formatC(df_error, digits = 2, format = "f"),
                          formatC(df_error, digits = 0, format = "f"))) %>%
      mutate(p_value = report_p_value(p_value)) %>%
      select(df_error, statistic, p_value)

    output <- with(res, paste0("*t*(", df_error, ") = ", statistic, ", ", p_value))
  }

  return(output)
}
