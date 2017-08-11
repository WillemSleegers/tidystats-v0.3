#' Report method for correlations
#'
#' Function to report a correlation in APA style.
#'
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
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
report_correlation <- function(results, identifier, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check if only a single statistic is asked, otherwise produce a full line of APA results
  if (!is.null(statistic)) {
    output <- res$value[res$statistic == statistic]

    if (statistic == "p") {
      output <- formatC(output, digits = 3, format = "f")
    } else {
      if (statistic != "df") {
        output <- formatC(output, digits = 2, format = "f")
      }
    }

  } else {
    if (grepl("Pearson", res$method[1])) {
      cor <- formatC(res$value[res$statistic == "cor"], digits = 2, format = "f")
      df <- res$value[res$statistic == "df"]
      p <- report_p_value(res$value[res$statistic == "p"])

      output <- paste0("*r*(", df, ") = ", cor, ", ", p)
    } else {
      if (grepl("Kendall", res$method[1])) {
        tau <- formatC(res$value[res$statistic == "tau"], digits = 2, format = "f")
        p <- report_p_value(res$value[res$statistic == "p"])

        output <- paste0("*r*<sub>$\\tau$</sub> = ", tau, ", ", p)
      } else {
        rho <- formatC(res$value[res$statistic == "rho"], digits = 2, format = "f")
        p <- report_p_value(res$value[res$statistic == "p"])

        output <- paste0("*r*<sub>*s*</sub> = ", rho, ", ", p)
      }
    }
  }

  return(output)
}
