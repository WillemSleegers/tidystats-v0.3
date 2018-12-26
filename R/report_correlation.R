#' Report function for correlations
#'
#' Function to report a correlation in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating the term you want to report.
#' @param term_nr A number indicating the term you want to report.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "correlation_pearson")
#' report(results, identifier = "correlation_spearman")
#' report(results, identifier = "correlation_kendall")
#'
#' @export

report_correlation <- function(results, identifier, term, term_nr) {

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

  # Extract statistics
  method <- first(pull(res, method))

  if (str_detect(method, "Pearson")) {
    r   <- pull(filter(res, statistic == "r"), value)
    df  <- pull(filter(res, statistic == "df"), value)
    p   <- pull(filter(res, statistic == "p"), value)

    r   <- report_statistic("r", r)
    p   <- report_p_value(p)

    output <- paste0("*r*(", df, ") = ", r, ", ", p)
  } else if (str_detect(method, "Kendall")) {
    tau <- pull(filter(res, statistic == "tau"), value)
    p   <- pull(filter(res, statistic == "p"), value)

    tau <- report_statistic("tau", tau)
    p   <- report_p_value(p)

    # τ = \u03C4
    output <- paste0("*r*~*\u03C4*~ = ", tau, ", ", p)

  } else if (str_detect(method, "Spearman")) {
    rho <- pull(filter(res, statistic == "rho"), value)
    p   <- pull(filter(res, statistic == "p"), value)

    rho <- report_statistic("rho", rho)
    p   <- report_p_value(p)

    output <- paste0("*r*~*s*~ = ", rho, ", ", p)
  }

  return(output)
}
