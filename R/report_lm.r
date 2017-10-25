#' Report method for linear regression models
#'
#' Function to report a regression in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @export

report_lm <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether a term is provided, extract data if so, otherwise throw an error
  if (!is.null(term)) {
    res <- res[res$term == term, ]
  } else if (!is.null(term_nr)) {
    res <- res[res$term_nr == term_nr, ]
  } else {
    stop("No term provided")
  }

  # Check if only a single statistic is asked, otherwise produce a full line of APA results
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
      if (statistic != "df") {
        output <- format(output, digits = 2, nsmall = 2)
      }
    }
  } else {
    if (res$term[1] == "(Model)") {
      adj_r <- gsub(pattern = "0\\.", replacement = ".",
                    x = format(res$value[res$statistic == "adjusted R squared"],
                               digits = 2, nsmall = 2))
      f <- format(res$value[res$statistic == "F"], digits = 2, nsmall = 2)
      df_num <- res$value[res$statistic == "numerator df"]
      df_den <- res$value[res$statistic == "denominator df"]
      p <- report_p_value(res$value[res$statistic == "p"])

      output <- paste0("adjusted *R*<sup>2</sup> = ", adj_r, ", *F*(", df_num, ", ",
                            df_den, ") = ", f, ", ", p)
    } else {
      b <- format(res$value[res$statistic == "b"], digits = 2, nsmall = 2)
      SE <- format(res$value[res$statistic == "SE"], digits = 2, nsmall = 2)
      t <- format(res$value[res$statistic == "t"], digits = 2, nsmall = 2)
      df <- res$value[res$statistic == "df"]
      p = report_p_value(res$value[res$statistic == "p"])

      output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ", t, ", ", p)
    }
  }

  return(output)
}
