#' Report method for ANOVA models
#'
#' Function to report ANOVA output in APA style.
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
#' # Set the list as the default list
#' options(tidystats_list = results)
#'
#' # Report results
#' report_anova("aov_two_way", term = "condition")
#' report_anova("aov_two_way", term = "sex")
#'
#' @export
report_anova <- function(identifier, group = NULL, term = NULL, term_nr = NULL,
  results = getOption("tidystats_list")) {

  output <- NULL

  # Extract model results
  res <- results[[identifier]]

  # Rename argument names to names that are not in res
  res_group <- group
  res_term <- term
  res_term_nr <- term_nr

  # Figure out both the term and term_nr
  res_term <- ifelse(!is.null(term), term, unique(dplyr::pull(res, term))[term_nr])
  res_term_nr <- ifelse(!is.null(term_nr), term_nr,
    first(dplyr::pull(dplyr::filter(res, term == res_term), term_nr)))

  # If the statistics of the residuals are requested, throw an error
  if (res_term == "Residuals") {
    stop(paste("APA output for residuals is not supported. Try reporting a",
      "single residual statistic instead"))
  }

  # Filter the results based on the supplied information
  if (!is.null(group)) {
    res <- dplyr::filter(res, group == res_group)
  }

  res <- dplyr::filter(res, term == res_term | term == "Residuals")

  if (nrow(res) == nrow(filter(res, term == "Residuals"))) {
    stop("No statistics found; did you supply the correct information?")
  }

  # Check if all the necessary statistics are there to produce a line of output
  if (sum(c("F", "p", "df") %in% unique(res$statistic)) == 3) {
    # TODO: Add a check for both num df and denominator df
    # Extract the dfs and then filter out irrelevant statistics
    dfs <- res %>%
      dplyr::filter(statistic == "df" & term_nr >= res_term_nr &
          (term == res_term | term == "Residuals")) %>%
      dplyr::pull(value)

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
