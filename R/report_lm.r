#' Report method for linear regression models
#'
#' Function to report a regression in APA style.
#'
#' @param identifier A character string identifying the model.
#' @param group A character string indicating the group containing the
#' statistics you want to report.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.
#' @param results A tidystats list.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the default results list
#' options(tidystats_list = results)
#'
#' # Report results
#' report("lm_simple", term = "conditionmortality salience")
#' report("lm_simple", term_nr = 2)
#' report("lm_simple", group = "model")
#'
#' @export
report_lm <- function(identifier, group = NULL, term = NULL, term_nr = NULL,
  results = getOption("tidystats_list")) {

  output <- NULL

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Store the arguments in variables that do not share column names with the
  # model data frame
  res_group <- group
  res_term <- term
  res_term_nr <- term_nr

  # Filter the results based on the supplied information
  if (!is.null(group)) {
    res <- dplyr::filter(res, group == res_group)
  }
  if (!is.null(term)) {
    res <- dplyr::filter(res, term == res_term)
  }
  if (!is.null(term_nr)) {
    res <- dplyr::filter(res, term_nr == res_term_nr)
  }

  if (nrow(res) == 0) {
    stop("No statistics found; did you supply the correct information?")
  }

  if (first(pull(res, group) == "model")) {
    # Check if all the necessary statistics are there to produce a line of
    # output
    if (sum(c("adjusted R squared", "F", "numerator df", "denominator df",
      "p") %in% unique(res$statistic)) == 5) {

      # TODO: Perhaps make adjusted R squared optional

      # Extract statistics
      adj_r  <- dplyr::pull(dplyr::filter(res,
        statistic == "adjusted R squared"), value)
      f <- dplyr::pull(dplyr::filter(res, statistic == "F"), value)
      df_num <- dplyr::pull(dplyr::filter(res, statistic == "numerator df"),
        value)
      df_den <- dplyr::pull(dplyr::filter(res, statistic == "denominator df"),
        value)
      p <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

      adj_r <- report_statistic("adjusted R squared", adj_r)
      f <- report_statistic("F", f)
      p <- report_p_value(p)

      output <- paste0("adj. *R\u00B2* = ", adj_r, ", *F*(", df_num, ", ", df_den,
        ") = ", f, ", ", p)
    }

  } else {
    # Check if all the necessary statistics are there to produce a line of
    # output
    if (sum(c("b", "SE", "t", "df", "p") %in% unique(res$statistic)) == 5) {
      b <- dplyr::pull(dplyr::filter(res, statistic == "b"), value)
      SE <- dplyr::pull(dplyr::filter(res, statistic == "SE"), value)
      t <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
      df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
      p <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

      b <- report_statistic("b", b)
      SE <- report_statistic("SE", SE)
      t <- report_statistic("t", t)
      p <- report_p_value(p)

      output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ",
        t, ", ", p)

      # Guess whether confidence intervals are included
      res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[1234567890]% CI"))

      # Add confidence interval, if it exists
      if ("[0-9]% CI" %in% dplyr::pull(res, statistic)) {
        res_CI <- dplyr::filter(res, stringr::str_detect(statistic, "[0-9]+% CI"))

        CI_pct <- readr::parse_number(first(pull(res_CI, statistic)))

        CI_lower <- dplyr::pull(res_CI, value)[1]
        CI_upper <- dplyr::pull(res_CI, value)[2]

        CI_lower <- report_statistic("CI", CI_lower)
        CI_upper <- report_statistic("CI", CI_upper)

        CI <- paste0(CI_pct, "% CI ", "[", CI_lower, ", ", CI_upper, "]")

        output <- paste0(output, ", ", CI)
      }
    }
  }

  return(output)
}

