#' Report method for a t-test
#'
#' Function to report a t-test in APA style.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export

report_t_test <- function(results, identifier, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Check if only a single statistic is asked; if not, produce a full line of APA results
  if (!is.null(statistic)) {

    output <- res$value[res$statistic == statistic]

    if (statistic == "p") {
      if (output < .001) {
        output <- "< .001"
      } else {
        output <- gsub(pattern = "0\\.", replacement = ".",
                       x = format(output, digits = 2, nsmall = 2))
      }
    } else {
      if (statistic != "df" | grepl("Welch", res$method[1])) {
        output <- format(output, digits = 2, nsmall = 2)
      }
    }
  } else {
    t <- format(res$value[res$statistic == "t"], digits = 2, nsmall = 2)

    if (grepl("Welch", res$method[1])) {
      df <- format(res$value[res$statistic == "df"], digits = 2, nsmall = 2)
    } else {
      df <- format(res$value[res$statistic == "df"], digits = 0)
    }

    p <- report_p_value(res$value[res$statistic == "p"])

    output <- paste0("*t*(", df, ") = ", t, ", ", p)
  }

  return(output)
}
