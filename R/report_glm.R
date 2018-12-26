#' Report method for generalized linear models
#'
#' Function to report a generalized linear models in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param group A character string indicating the group containing the statistic
#' or statistics you want to report.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.
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
#' @import dplyr
#' @import stringr
#'
#' @export
report_glm <- function(results, identifier, group = NULL, term = NULL,
  term_nr = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether the group is 'model'; if so, throw an unsupported error
  if (!is.null(group)) {
    if (group == "model") {
      stop("Unsupported; try reporting one statistic at a time")
    }
  }

  # TODO: Check whether a logistic regression is done. If so, also calculate OR

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

  output <- NULL

  # Check if enough information has been provided to produce a single line of
  # output
  if (length(unique(res$term)) > 1) {
    stop("Not enough information supplied.")
  }

  # Check if all the necessary statistics are there to produce a line of output
  if (sum(c("b", "SE", "t", "z", "p", "df") %in% unique(res$statistic)) == 5) {

    b <- dplyr::pull(dplyr::filter(res, statistic == "b"), value)
    SE <- dplyr::pull(dplyr::filter(res, statistic == "SE"), value)
    df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
    p <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

    b <- report_statistic("b", b)
    SE <- report_statistic("SE", SE)
    p <- report_p_value(p)

    if ("z" %in% unique(res$statistic)) {
      z <- dplyr::pull(dplyr::filter(res, statistic == "z"), value)
      z <- report_statistic("z", z)

      output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *z*(",  df, ") = ",
        z, ", ", p)
    } else {
      t <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
      t <- report_statistic("t", t)

      output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ",
        t, ", ", p)
    }

    # Guess whether confidence intervals are included
    res_CI <- dplyr::filter(res, str_detect(statistic, "[1234567890]% CI"))

    # Add confidence interval, if it exists
    if ("[0-9]% CI" %in% dplyr::pull(res, statistic)) {
      res_CI <- filter(res, str_detect(statistic, "[0-9]+% CI"))

      CI_pct <- parse_number(first(pull(res_CI, statistic)))

      CI_lower <- pull(res_CI, value)[1]
      CI_upper <- pull(res_CI, value)[2]

      CI_lower <- report_statistic("CI", CI_lower)
      CI_upper <- report_statistic("CI", CI_upper)

      CI <- paste0(CI_pct, "% CI ", "[", CI_lower, ", ", CI_upper, "]")

      output <- paste0(output, ", ", CI)
    }
  }

  return(output)
}

