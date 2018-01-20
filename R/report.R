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

report <- function(identifier, term = NULL, term_nr = NULL, var = NULL, group = NULL,
                   statistic = NULL, results = getOption("tidystats_list")) {

  # Check whether the identifier exists
  if (!identifier %in% names(results)) {
    stop("Identifier not found.")
  } else {
    res <- results[[identifier]]
  }

  if ("method" %in% names(res)) {
    method <- res$method[1]

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
  }

  # If output is null, it means we either do not have the report function for that method, or the
  # results are descriptives. In this case we can report a single statistic if enough information
  # is provided.
  if (is.null(output)) {
    res_statistic <- statistic

    if (is.null(res_statistic)) {
      stop("Cannot report multiple statistics, please provide a single statistic.")
    } else {
      if ("var" %in% names(res)) {
        res_var <- var
        if (length(unique(res$var)) > 1 & is.null(res_var)) {
          stop("var information required.")
        } else {
          if ("group" %in% names(res)) {
            res_group <- group
            if (is.null(res_group)) {
              stop("group information required.")
            } else {
              if (length(unique(res$var)) == 1 & "group" %in% names(res)) {
                output <- format(filter(res, statistic == res_statistic & group == res_group)$value,
                                 digits = 2, nsmall = 2)
                } else {
                  output <- format(filter(res, statistic == res_statistic & var == res_var &
                                            group == res_group)$value, digits = 2, nsmall = 2)
                  }
                }
          } else {
            if (length(unique(res$var)) == 1) {
              output <- format(filter(res, statistic == res_statistic)$value, digits = 2,
                               nsmall = 2)
            } else {
              output <- format(filter(res, statistic == res_statistic & var == res_var)$value,
                                 digits = 2, nsmall = 2)
              }
            }
          }
        } else {
        if (length(unique(res$term)) > 1 & (is.null(term) & is.null(term_nr))) {
          print("term information required.")
        } else {
          if (length(unique(res$term)) == 1) {
            output <- format(filter(res, statistic == res_statistic)$value, digits = 2, nsmall = 2)
          } else {
            if (!is.null(term)) {
              res_term <- term
              output <- format(filter(res, term == res_term & statistic == res_statistic)$value,
                               digits = 2, nsmall = 2)
            } else {
              res_term_nr <- term_nr
              output <- format(
                filter(res, term_nr == res_term_nr & statistic == res_statistic)$value,
                digits = 2, nsmall = 2)
            }
          }
        }
      }
    }
  }

  return(output)
}

