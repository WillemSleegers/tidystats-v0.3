#' Report method for ANOVA models
#'
#' Function to report ANOVA output in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the
#' statistics of.
#' @param term_nr A number indicating which term you want to report the the
#' statistics of.
#' @param statistic A character string of a statistic you want to extract from a
#' model.
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
#' @import stringr
#'
#' @export

report_anova <- function(results, identifier, term = NULL, term_nr = NULL,
                         statistic = NULL) {

  # Check whether a term, term_nr, or statistic has been provided
  if (is.null(term) & is.null(term_nr) & is.null(statistic)) {
    stop("Please provide a term, term number, or statistic.")
  }

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check if only a single statistic is asked, otherwise produce a full line of
  # APA results
  if (!is.null(statistic)) {

    # Check whether the statistic exists
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    }

    # Select statistics of the term, if provided
    if (!is.null(term)) {
      res <- res[res$term == term, ]
    } else if (!is.null(term_nr)) {
      res <- res[res$term_nr == term_nr, ]
    }

    # Get the value of the statistic
    value <- res$value[res$statistic == statistic]

    # Check whether enough information was supplied by checking whether the
    # value vector contains more than 1 element
    if (length(value) > 1) {
      stop(paste("Not enough information provided, considering adding term",
                 "information."))
    }

    output <- report_statistic(statistic, value)
  } else {

    # Get the provided information
    res_term <- ifelse(!is.null(term), term, unique(res$term)[term_nr])
    res_term_nr <- ifelse(!is.null(term_nr), term_nr,
                          res$term_nr[res$term == res_term][1])

    # If the statistics of the residuals are requested, throw an error
    if (res_term == "Residuals" | str_detect(res_term, "_Residuals")) {
      stop(paste("APA output for residuals is not supported. Try reporting a",
                 "single residual statistic instead."))
    }

    # Extract the denominator df and then filter out irrelevant statistics
    df_den <- res$value[res$term_nr == res_term_nr + 1 & res$statistic == "df"]
    res <- res[res$term == res_term, ]

    f      <- report_statistic("F", res$value[res$statistic == "F"])
    df_num <- report_statistic("df", res$value[res$statistic == "df"])
    df_den <- report_statistic("df", df_den)
    p      <- report_statistic("p", res$value[res$statistic == "p"])

    output <- paste0("*F*(", df_num, ", ", df_den, ") = ", f, ", ", p)
    }

  return(output)
}
