#' Report method for linear regression models
#'
#' Function to report a regression in APA style.
#'
#' @examples
#' results <- new_stats_data_frame()
#'
#' lm_test <- lm(extra ~ sleep, data = data)
#'
#' results <- add_stats(results, lm_test, "lm_test1")
#' report_lm(results, "lm_test1")
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
report_lm <- function(results, identifier, term, statistic = NULL) {

  # Extract the results of the specific model through its identifier and term
  res <- results[[identifier]]
  res <- res[res$term == term, ]

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
    if (term == "(Model)") {
      adj_r <- formatC(res$value[res$statistic == "adjusted R squared"], digits = 2, format = "f")
      f <- formatC(res$value[res$statistic == "F"], digits = 2, format = "f")
      df_num <- res$value[res$statistic == "numerator df"]
      df_den <- res$value[res$statistic == "denominator df"]
      p <- report_p_value(res$value[res$statistic == "p"])

      output <- paste0("adjusted *R*<sup>2</sup> = ", adj_r, ", *F*(", df_num, ", ",
                            df_den, ") = ", f, ", ", p)
    } else {
      b <- formatC(res$value[res$statistic == "b"], digits = 2, format = "f")
      SE <- formatC(res$value[res$statistic == "SE"], digits = 2, format = "f")
      t <- formatC(res$value[res$statistic == "t"], digits = 2, format = "f")
      df <- res$value[res$statistic == "df"]
      p = report_p_value(res$value[res$statistic == "p"])

      output <- paste0("*b* = ", b, ", *SE* = ", SE, ", *t*(",  df, ") = ", t, ", ", p)
    }
  }

  return(output)
}
