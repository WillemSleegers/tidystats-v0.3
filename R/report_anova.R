#' Report method for ANOVA models
#'
#' Function to report ANOVA output in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "ANOVA", term = "N")
#' report(results, identifier = "ANOVA", term_nr = "2")
#' report(results, identifier = "ANOVA", term = "N", statistic = "p")
#'
#' @export

report_anova <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether a term is provided, extract data if so, otherwise throw an error
  # Also store the residuals degree of freedom
  if (!is.null(term)) {
    df_den <- res$value[grepl("Residuals", res$term) & res$statistic == "df" &
                          res$term_nr > res$term_nr[res$term == term][1]]
    res <- res[res$term == term, ]
  } else if (!is.null(term_nr)) {
    df_den <- res$value[grepl("Residuals", res$term) & res$term_nr > term_nr &
                          res$statistic == "df"][1]
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
