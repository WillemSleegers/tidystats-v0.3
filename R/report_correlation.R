#' Report function for correlations
#'
#' Function to report a correlation in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param statistic A character string identifying the exact statistic you want
#' to report.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Report results
#' report(results, identifier = "correlation")
#' report(results, identifier = "correlation", statistic = "p")
#'
#' @export

report_correlation <- function(results, identifier, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether a single statistic is requested, or a full line of APA output
  if (!is.null(statistic)) {

    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    } else {
      res_statistic <- statistic
    }

    value  <- pull(filter(res, statistic == res_statistic), value)
    output <- report_statistic(res_statistic, value)
  } else {
    method <- first(pull(res, method))

    if (str_detect(method, "Pearson")) {
      cor <- pull(filter(res, statistic == "cor"), value)
      df  <- pull(filter(res, statistic == "df"), value)
      p   <- pull(filter(res, statistic == "p"), value)

      cor <- report_statistic("cor", cor)
      p   <- report_p_value(p)

      output <- paste0("*r*(", df, ") = ", cor, ", ", p)
    } else if (str_detect(method, "Kendall")) {
      tau <- pull(filter(res, statistic == "tau"), value)
      p   <- pull(filter(res, statistic == "p"), value)

      tau <- report_statistic("tau", tau)
      p   <- report_p_value(p)

      output <- paste0("*r*~τ~ = ", tau, ", ", p)
    } else if (str_detect(method, "Spearman")) {
      rho <- pull(filter(res, statistic == "rho"), value)
      p   <- pull(filter(res, statistic == "p"), value)

      rho <- report_statistic("rho", rho)
      p   <- report_p_value(p)

      output <- paste0("*r*~*s*~ = ", rho, ", ", p)
    } else {
      stop("Not supported.")
    }
  }

  return(output)
}
