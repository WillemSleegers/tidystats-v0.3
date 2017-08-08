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

  # Extract model results
  res <- results[[identifier]]

  # Check if only a single statistic is asked
  # If not, produce a full line of APA results
  if (!is.null(statistic)) {
    output <- pull(res[statistic])

    if (statistic == "df_error" & !grepl("Welch", res$method)) {
      output <- formatC(output, digits = 0, format = "d")
    } else {
      output <- formatC(output, digits = 2, format = "f")
    }
  } else {
    statistic <- formatC(res$statistic, digits = 2, format = "f")
    
    if (grepl("Welch", method)) {
      df_error <- formatC(df_error, digits = 2, format = "f")
    } else {
      df_error <- formatC(df_error, digits = 0, format = "d")
    }
    
    p_value <- report_p_value(res$p_value)
    
    output <- paste0("*t*(", df_error, ") = ", statistic, ", ", p_value)
  }

  return(output)
}
