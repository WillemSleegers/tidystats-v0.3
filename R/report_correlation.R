#' Report function for correlations
#'
#' Function to report a correlation in APA style.
#'
#' @param identifier A character string identifying the model.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.]
#' @param results A tidystats list.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the default tidystats list in options()
#' options(tidystats_list = results)
#'
#' # Report results
#' report(identifier = "correlation_pearson")
#' report(identifier = "correlation_spearman")
#' report(identifier = "correlation_kendall")
#'
#' @export

report_correlation <- function(identifier, term, term_nr,
  results = getOption("tidystats_list")) {
  output <- NULL

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Store the arguments in variables that do not share column names with the
  # model data frame
  res_term <- term
  res_term_nr <- term_nr

  # Filter the results based on the supplied information
  if (!is.null(term)) {
    res <- dplyr::filter(res, term == res_term)
  }
  if (!is.null(term_nr)) {
    res <- dplyr::filter(res, term_nr == res_term_nr)
  }

  if (nrow(res) == 0) {
    stop("No statistics found; did you supply the correct information?")
  }

  # Extract the method to determine the type of correlation
  method <- first(pull(res, method))

  if (stringr::str_detect(method, "Pearson")) {
    # Check if all the necessary statistics are there to produce a line of
    # output
    if (sum(c("r", "df", "p") %in% unique(res$statistic)) == 3) {
      # Extract statistics
      r   <- dplyr::pull(filter(res, statistic == "r"), value)
      df  <- dplyr::pull(filter(res, statistic == "df"), value)
      p   <- dplyr::pull(filter(res, statistic == "p"), value)

      r   <- report_statistic("r", r)
      p   <- report_p_value(p)

      output <- paste0("*r*(", df, ") = ", r, ", ", p)
    }
  } else if (stringr::str_detect(method, "Kendall")) {
    # Check if all the necessary statistics are there to produce a line of
    # output
    if (sum(c("tau", "p") %in% unique(res$statistic)) == 2) {
      # Extract statistics
      tau <- dplyr::pull(filter(res, statistic == "tau"), value)
      p   <- dplyr::pull(filter(res, statistic == "p"), value)

      tau <- report_statistic("tau", tau)
      p   <- report_p_value(p)

      output <- paste0("*r*~*\u03C4*~ = ", tau, ", ", p)
    }
  } else if (stringr::str_detect(method, "Spearman")) {
    # Check if all the necessary statistics are there to produce a line of
    # output
    if (sum(c("rho", "p") %in% unique(res$statistic)) == 2) {
      # Extract statistics
      rho <- dplyr::pull(filter(res, statistic == "rho"), value)
      p   <- dplyr::pull(filter(res, statistic == "p"), value)

      rho <- report_statistic("rho", rho)
      p   <- report_p_value(p)

      output <- paste0("*r*~*s*~ = ", rho, ", ", p)
    }
  }

  return(output)
}
