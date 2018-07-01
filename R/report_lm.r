#' Report method for linear regression models
#'
#' Function to report a regression in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param group A character string indicating the group containing the statistic
#' or statistics you want to report.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.
#' @param statistic A character string identifying the exact statistic you want
#' to report.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the default results list
#' options(tidystats_list = results)
#'
#' # Example: regression term
#' report("regression", term = "groupTrt")
#' report("regression", term_nr = 2)
#' report("regression", term = "groupTrt", statistic = "p")
#'
#' @import dplyr
#' @import stringr
#'
#' @export
report_lm <- function(results, identifier, group = NULL, term = NULL,
                      term_nr = NULL, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Store the arguments in variables that do not share column names with the
  # model data frame
  res_group <- group
  res_term <- term
  res_term_nr <- term_nr
  res_statistic <- statistic

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
  if (!is.null(statistic)) {
    res <- dplyr::filter(res, statistic == res_statistic)
  }

  # Check if only a single statistic is asked, otherwise produce a full line of
  # APA results
  if (!is.null(statistic)) {

    # Check whether the statistic exists
    if (!statistic %in% dplyr::pull(res, statistic)) {
      stop("Statistic not found.")
    }

    # Get the value of the statistic
    value <- dplyr::pull(res, value)
    output <- report_statistic(res_statistic, value)
  } else {
    if (first(pull(res, group) == "model")) {
      adj_r  <- dplyr::pull(dplyr::filter(res,
                                          statistic == "adjusted R squared"),
                            value)
      f      <- dplyr::pull(dplyr::filter(res, statistic == "F"), value)
      df_num <- dplyr::pull(dplyr::filter(res, statistic == "numerator df"),
                            value)
      df_den <- dplyr::pull(dplyr::filter(res, statistic == "denominator df"),
                            value)
      p      <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

      adj_r  <- report_statistic("adjusted R squared", adj_r)
      f      <- report_statistic("F", f)
      p      <- report_p_value(p)

      output <- paste0("adjusted *R*^2^ = ", adj_r, ", *F*(", df_num, ", ",
                       df_den, ") = ", f, ", ", p)
    } else {
      b  <- dplyr::pull(dplyr::filter(res, statistic == "b"), value)
      SE <- dplyr::pull(dplyr::filter(res, statistic == "SE"), value)
      t  <- dplyr::pull(dplyr::filter(res, statistic == "t"), value)
      df <- dplyr::pull(dplyr::filter(res, statistic == "df"), value)
      p  <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)

      b  <- report_statistic("b", b)
      SE <- report_statistic("SE", SE)
      t  <- report_statistic("t", t)
      p  <- report_p_value(p)

      # Guess whether confidence intervals are included
      res_CI <- dplyr::filter(res, str_detect(statistic, "[1234567890]% CI"))

      if (nrow(res_CI) > 0) {
        CI_pct <- as.numeric(stringr::str_replace(
          dplyr::pull(res_CI, statistic), "% CI", ""))
        CI_pct <- CI_pct[2] - CI_pct[1]

        CI_value1 <- dplyr::pull(res_CI, value)[1]
        CI_value2 <- dplyr::pull(res_CI, value)[2]

        CI_value1 <- report_statistic("CI", CI_value1)
        CI_value2 <- report_statistic("CI", CI_value2)

        CI <- paste0(CI_pct, "% CI ", "[", CI_value1, ", ", CI_value2, "]")

        output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ",
                         t, ", ", p, ", ", CI)
      } else {
        output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ",
                         t, ", ", p)
      }
    }
  }

  # Check whether enough information was supplied by checking whether the output
  # vector contains more than 1 element
  if (length(output) > 1) {
    stop(paste("Not enough information provided, considering adding group/term",
               "information."))
  }

  return(output)
}

