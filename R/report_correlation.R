#' Report method for correlations
#'
#' Function to report a correlation in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "correlation")
#' report(results, identifier = "correlation", statistic = "r")
#'
#' @export

report_correlation <- function(results, identifier, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether the statistic exists, if provided
  if (!is.null(statistic) & !statistic %in% res$statistic) {
    stop("Statistic not found.")
  }

  # Check if only a single statistic is asked, otherwise produce a full line of APA results
  if (!is.null(statistic)) {
    output <- res$value[res$statistic == statistic]

    if (statistic != "df") {
      output <- format(output, digits = 2, nsmall = 2)
    }

    if (statistic %in% c("cor", "tau", "rho")) {
      output <- gsub(pattern = "0\\.", replacement = ".",
                     x = format(output, digits = 2, nsmall = 2))
    }

  } else {
    if (grepl("Pearson", res$method[1])) {
      cor <- gsub(pattern = "0\\.", replacement = ".",
                  x = format(res$value[res$statistic == "cor"], digits = 2, nsmall = 2))
      df <- res$value[res$statistic == "df"]
      p <- report_p_value(res$value[res$statistic == "p"])

      output <- paste0("*r*(", df, ") = ", cor, ", ", p)
    } else {
      if (grepl("Kendall", res$method[1])) {
        tau <- gsub(pattern = "0\\.", replacement = ".",
                    x = format(res$value[res$statistic == "tau"], digits = 2, nsmall = 2))
        p <- report_p_value(res$value[res$statistic == "p"])

        output <- paste0("*r*<sub>$\\tau$</sub> = ", tau, ", ", p)
      } else {
        rho <- gsub(pattern = "0\\.", replacement = ".",
                    x = format(res$value[res$statistic == "rho"], digits = 2, nsmall = 2))
        p <- report_p_value(res$value[res$statistic == "p"])

        output <- paste0("*r*<sub>*s*</sub> = ", rho, ", ", p)
      }
    }
  }

  return(output)
}
