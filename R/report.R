#' Report function
#'
#' \code{report} is a general function that returns Markdown code of a statistical test in 6th edition APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param group A character string identifying the group.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#' @param var A character string identifying the variable.
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
#' report("t_test_one_sample")
#' report("t_test_welch")
#'
#' # Example: correlation
#' report("correlation_pearson")
#' report("correlation_spearman")
#'
#' # Example: ANOVA
#' report("aov_two_way", term = "condition")
#' report("aov_two_way", term = "sex")
#'
#' # Example: Linear models
#' report("lm_simple", term = "conditionmortality salience")
#' report("lm_simple", term_nr = 2)
#' report("lm_simple", group = "model")
#'
#' @export

report <- function(identifier, term = NULL, term_nr = NULL, var = NULL,
  group = NULL, statistic = NULL, results = getOption("tidystats_list")) {

  # Check whether the results list is provided
  if (is.null(results)) {
    stop("No results found; did you specify a results list?")
  }

  # Check whether the identifier exists, otherwise extract it
  if (!identifier %in% names(results)) {
    stop("Identifier not found.")
  } else {
    res <- results[[identifier]]
  }

  output <- NULL

  # Check whether a single statistic is requested, or a full line of output
  if (is.null(statistic)) {
    # Run the appropriate reporting function based on the method information
    if ("method" %in% names(res)) {
      method <- res$method[1]

      if (stringr::str_detect(method, "t-test")) {
        output <- report_t_test(identifier, results = results)
      } else if (stringr::str_detect(method, "Chi-squared")) {
        output <- report_chi_squared(identifier, results = results)
      } else if (stringr::str_detect(method, "Wilcoxon")) {
        output <- report_wilcoxon(identifier, results = results)
      } else if (stringr::str_detect(method, "Fisher")) {
        output <- report_fisher(identifier, results = results)
      } else if (stringr::str_detect(method, "correlation")) {
        output <- report_correlation(identifier, term, term_nr,
          results = results)
        # Check for GLMs before checking for LMs
      } else if (stringr::str_detect(method, "Generalized linear model")) {
        output <- report_glm(identifier, group, term, term_nr,
          results = results)
      } else if (stringr::str_detect(method, "(L|l)inear model")) {
        output <- report_lm(identifier, group, term, term_nr, results = results)
      } else if (stringr::str_detect(method, "(L|l)inear mixed model")) {
        output <- report_lmm(identifier, group, term, term_nr,
          results = results)
      } else if (stringr::str_detect(method, "ANOVA|ANCOVA")) {
        output <- report_anova(identifier, group, term, term_nr,
          results = results)
      }
    }
  }

  # If output is null, this is either because we do not yet have support for the
  # method, or a single statistic is requested. In thcan report a single statistic if enough information is provided.
  if (is.null(output)) {

    # Filter: term
    if (!is.null(term)) {
      res_term <- term

      if (!res_term %in% unique(res$term)) {
        stop("Term not found.")
      }

      res <- dplyr::filter(res, term == res_term)
    }

    # Filter: term_nr
    if (!is.null(term_nr)) {
      res_term_nr <- term_nr

      if (!res_term_nr %in% unique(res$term_nr)) {
        stop("Term number not found.")
      }

      res <- dplyr::filter(res, term_nr == res_term_nr)
    }

    # Filter: statistic
    if (!is.null(statistic)) {
      res_statistic <- statistic

      if (!res_statistic %in% unique(res$statistic)) {
        stop("Statistic not found.")
      }

      res <- dplyr::filter(res, statistic == res_statistic)
    }

    # Filter: group
    if (!is.null(group)) {
      res_group <- group

      if (!res_group %in% unique(res$group)) {
        stop("Group not found.")
      }

      res <- dplyr::filter(res, group == res_group)
    }

    # Filter: var
    if (!is.null(var)) {
      res_var <- var

      if (!res_var %in% unique(res$var)) {
        stop("Variable not found.")
      }

      res <- dplyr::filter(res, var == res_var)
    }

    # Check if enough information is provided
    info <- dplyr::select(res, contains("var"), contains("group"),
      contains("statistic"), contains("term"))

    for (column in names(info)) {

      if (length(unique(dplyr::pull(info, column))) > 1) {

        stop(paste("Not enough information provided. Please provide", column,
          "information."))
      }
    }

    # Extract statistic
    if (res$statistic[1] == "p") {
      output <- report_p_value(res$value[1])
    } else {

      # Check if the value is an integer, else return a string with 2
      # significant digits
      if (res$value[1] %% 1 == 0) {
        output <- prettyNum(res$value[1])
      } else {
        output <- format(res$value[1], digits = 2, nsmall = 2)
      }
    }
  }

  return(output)
}

