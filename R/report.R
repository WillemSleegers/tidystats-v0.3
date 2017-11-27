#' Report function
#'
#' \code{report} is a general function that returns Markdown code of a statistical test in 6th edition APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @details \code{report} calls a specific report function dependent on the type of statistical test that is supplied. The 'method' column of the statistical test is used to determine which report function to run.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Example: t-test
#' report(results, identifier = "t_test")
#'
#' # Example: correlation, r statistic only
#' report(results, identifier = "correlation", statistic = "r")
#'
#' # Example: regression term
#' report(results, identifier = "regression", term = "groupTrt")
#' report(results, identifier = "regression", term_nr = 2)
#'
#' # Example: ANOVA
#' report(results, identifier = "ANOVA", term = "N")
#'
#' @import dplyr
#'
#' @export

report <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Check whether the identifier exists
  if (!identifier %in% names(results)) {
    stop("Identifier not found.")
  }

  res_term <- term
  res_term_nr <- term_nr
  res_statistic <- statistic

  # Find out which test was used
  res <- results[[identifier]]
  method <- res$method[1]

  # Check whether the statistic exists, if provided
  if (!is.null(statistic) & !statistic %in% res$statistic) {
    stop("Statistic not found.")
  }

  # Run the appropriate report function
  if (grepl("t-test", method)) {
    output <- report_t_test(results, identifier, statistic)
  } else {
    if (grepl("correlation", method)) {
      output <- report_correlation(results, identifier, statistic)
    } else {
      if (grepl("regression", method)) {
        output <- report_lm(results, identifier, term, term_nr, statistic)
      } else {
        if (grepl("ANOVA|ANCOVA", method)) {
          output <- report_anova(results, identifier, term, term_nr, statistic)
        } else {
          output <- NULL
        }
      }
    }
  }

  # If output is null, it means we do not yet have a reporting function
  # However, we can still report one statistic at the time
  if (is.null(output)) {
    if (!is.null(res_statistic)) {

      # Check if term information is required
      if ("term" %in% names(res)) {
        if (sum(!is.na(res$term)) > 0) {
          if (is.null(term) & is.null(term_nr)) {
            stop("No term information found.")
          } else {
            if (!is.null(term)) {
              output <- format(filter(res, term == res_term & statistic == res_statistic)$value,
                               digits = 2, nsmall = 2)
            } else {
              output <- format(
                filter(res, term_nr == res_term_nr & statistic == res_statistic)$value,
                digits = 2, nsmall = 2)
            }
          }
        } else {
          output <- format(filter(res, statistic == res_statistic)$value, digits = 2, nsmall = 2)
        }
      }
    }
  }

  # If it is still null, no statistic was supplied
  if (is.null(output)) {
    stop("Reporting for this method is not supported (but try reporting a single statistic at the time).")
  }

  return(output)
}

