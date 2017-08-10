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

  # Check if only a single statistic is asked; if not, produce a full line of APA results
  if (!is.null(statistic)) {

    output <- res$value[res$statistic == statistic]

    if (statistic == "p") {
      output <- formatC(output, digits = 3, format = "f")
    } else {
      if (statistic != "df" | grepl("Welch", res$method[1])) {
        output <- formatC(output, digits = 2, format = "f")
      }
    }

  } else {
    t <- formatC(res$value[res$statistic == "t"], digits = 2, format = "f")

    if (grepl("Welch", res$method[1])) {
      df <- formatC(res$value[res$statistic == "df"], digits = 2, format = "f")
    } else {
      df <- formatC(res$value[res$statistic == "df"], digits = 0, format = "d")
    }

    p <- report_p_value(res$value[res$statistic == "p"])

    output <- paste0("*t*(", df, ") = ", t, ", ", p)
  }

  return(output)
}
