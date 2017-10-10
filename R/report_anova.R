#' Report method for ANOVA models
#'
#' Function to report ANOVA output in APA style.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
report_anova <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether a term is provided, extract data if so, otherwise throw an error
  # Also store the residuals degree of freedom
  if (!is.null(term)) {
    df_den <- res$value[grepl("Residuals", res$term) & grepl(term, res$term) &
                          res$statistic == "df"][1]
    res <- res[res$term == term, ]
  } else if (!is.null(term_nr)) {
    df_den <- res$value[grepl("Residuals", res$term) & grepl(res$term[res$term_nr==term_nr][1],
                                                             res$term) & res$statistic == "df"][1]
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
      f <- format(res$value[res$statistic == "F"], digits = 2, nsmall = 2)
      df_num <- res$value[res$statistic == "df"]
      df_den <- df_den
      p = report_p_value(res$value[res$statistic == "p"])

      output <- paste0("*F*(", df_num, ", ", df_den, ") = ", f, ", ", p)
  }

  return(output)
}
