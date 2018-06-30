#' Report function
#'
#' \code{report} is a general function that returns Markdown code of a statistical test in 6th edition APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#' @param var A character string identifying the variable.
#' @param group A character string identifying the group/
#'
#' @details \code{report} calls a specific report function dependent on the type of statistical test that is supplied. The 'method' column of the statistical test is used to determine which report function to run.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the list as the default list
#' options(tidystats_list = results)
#'
#' # Example: t-test
#' report("t_test")
#'
#' # Example: correlation, r statistic only
#' report("correlation", statistic = "p")
#'
#' # Example: regression term
#' report("regression", term = "groupTrt")
#' report("regression", term_nr = 2)
#'
#' # Example: ANOVA
#' report("ANOVA", term = "N")
#'
#' @import dplyr
#'
#' @export

report <- function(identifier, term = NULL, term_nr = NULL, var = NULL, group = NULL,
                   statistic = NULL, results = getOption("tidystats_list")) {

  # Check whether the identifier exists, otherwise extract it
  if (!identifier %in% names(results)) {
    stop("Identifier not found.")
  } else {
    res <- results[[identifier]]
  }

  if ("method" %in% names(res)) {
    method <- res$method[1]

    # Run the appropriate report function
    if (str_detect(method, "t-test")) {
      output <- report_t_test(results, identifier, statistic)
    } else if (str_detect(method, "Chi-squared")) {
      output <- report_chi_squared(results, identifier, statistic)
    } else if (str_detect(method, "correlation")) {
      output <- report_correlation(results, identifier, statistic)
    } else if (str_detect(method, "regression")) {
      output <- report_lm(results, identifier, group, term, term_nr, statistic)
    } else if (str_detect(method, "ANOVA|ANCOVA")) {
      output <- report_anova(results, identifier, term, term_nr, statistic)
    } else if (str_detect(method, "metafor")) {
      output <- report_rma(results, identifier, group, term, term_nr, statistic)
    } else {
      output <- NULL
    }
  }

  # If output is null, it means we either do not have the report function for that method, or the
  # results are descriptives. In this case we can report a single statistic if enough information
  # is provided.
  if (is.null(output)) {

    # Filter: term
    if (!is.null(term)) {
      res_term <- term

      if (!res_term %in% unique(res$term)) {
        stop("Term not found.")
      }

      res <- filter(res, term == res_term)
    }

    # Filter: term_nr
    if (!is.null(term_nr)) {
      res_term_nr <- term_nr

      if (!res_term_nr %in% unique(res$term_nr)) {
        stop("Term number not found.")
      }

      res <- filter(res, term_nr == res_term_nr)
    }

    # Filter: statistic
    if (!is.null(statistic)) {
      res_statistic <- statistic

      if (!res_statistic %in% unique(res$statistic)) {
        stop("Statistic not found.")
      }

      res <- filter(res, statistic == res_statistic)
    }

    # Filter: group
    if (!is.null(group)) {
      res_group <- group

      if (!res_group %in% unique(res$group)) {
        stop("Group not found.")
      }

      res <- filter(res, group == res_group)
    }

    # Filter: var
    if (!is.null(var)) {
      res_var <- var

      if (!res_var %in% unique(res$var)) {
        stop("Variable not found.")
      }

      res <- filter(res, var == res_var)
    }

    # Check if enough information is provided
    info <- select(res, contains("var"), contains("group"), contains("statistic"), contains("term"))

    for (column in names(info)) {

      if (length(unique(pull(info, column))) > 1) {

        stop(paste("Not enough information provided. Please provide", column, "information."))
      }
    }

    # Extract statistic
    if (res$statistic[1] == "p") {
      output <- report_p_value(res$value[1])
    } else {

      # Check if the value is an integer, else return a string with 2 significant digits
      if (res$value[1] %% 1 == 0) {
        output <- prettyNum(res$value[1])
      } else {
        output <- format(res$value[1], digits = 2, nsmall = 2)
      }
    }
  }

  return(output)
}

