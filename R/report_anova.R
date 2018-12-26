#' Report method for ANOVA models
#'
#' Function to report ANOVA output in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.
#' @param statistic A character string identifying the exact statistic you want
#' to report.
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

report_anova <- function(results, identifier, group = NULL, term = NULL,
  term_nr = NULL, statistic = NULL) {

  # Extract model results
  res <- results[[identifier]]

  # Rename argument names to names that are not in res
  res_group <- group
  res_statistic <- statistic
  res_term <- term
  res_term_nr <- term_nr

  # Check whether a single statistic is requested, or a full line of APA output
  if (!is.null(statistic)) {

    # Check whether the statistic exists
    if (!statistic %in% unique(res$statistic)) {
      stop("statistic not found")
    }

    # Select statistics of the group, if provided
    if (!is.null(group)) {
      if (group %in% unique(res$group)) {
        res <- dplyr::filter(res, group == res_group)
      } else {
        stop("group not found")
      }
    }

    # Select statistics of the term, if provided
    if (!is.null(term)) {
      res <- dplyr::filter(res, term == res_term)
    } else if (!is.null(term_nr)) {

      # Coerce the term_nr to a number if it was provided as a string
      if (!is.numeric(term_nr)) {
        res_term_nr <- as.numeric(res_term_nr)
      }

      res <- dplyr::filter(res, term_nr == res_term_nr)
    }

    # Get the value of the statistic
    value <- res %>%
      dplyr::filter(statistic == res_statistic) %>%
      pull(value)

    # Check whether enough information was supplied by checking whether the
    # value vector contains more than 1 element
    if (length(value) > 1) {
      stop(paste("not enough information provided, considering adding group or",
        "term information"))
    }

    output <- report_statistic(res_statistic, value)
  } else {

    # Check whether a term or term_nr has been provided
    if (is.null(term) & is.null(term_nr)) {
      stop("Please provide a term or term number")
    }

    if (!is.null(term_nr) & !is.numeric(term_nr)) {
      stop("term_nr is not a number")
    }

    # Get the provided information
    res_term <- ifelse(!is.null(term), term, unique(pull(res, term))[term_nr])
    res_term_nr <- ifelse(!is.null(term_nr), term_nr,
                          first(pull(filter(res, term == res_term), term_nr)))


    # If the statistics of the residuals are requested, throw an error
    if (res_term == "Residuals") {
      stop(paste("APA output for residuals is not supported. Try reporting a",
        "single residual statistic instead"))
    }

    # Extract the dfs and then filter out irrelevant statistics
    dfs <- res %>%
      dplyr::filter(statistic == "df" & term_nr >= res_term_nr &
          (term == res_term | term == "Residuals")) %>%
      pull(value)

    res <- dplyr::filter(res, term == res_term)

    f      <- dplyr::pull(dplyr::filter(res, statistic == "F"), value)
    p      <- dplyr::pull(dplyr::filter(res, statistic == "p"), value)
    df_num <- dfs[1]
    df_den <- dfs[2]

    f      <- report_statistic("F", f)
    p      <- report_p_value(p)

    output <- paste0("*F*(", df_num, ", ", df_den, ") = ", f, ", ", p)
    }

  return(output)
}
