#' Report method for a t-test or correlation.
#'
#' Function to report a t-test or correlation in APA style.
#'
#' @examples
#' report.htest(results, "model1")
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
report.htest <- function(results, identifier, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check if only a single statistic is asked, otherwise produce full line of APA results
  if (!is.null(statistic)) {
    output <- res[statistic]
  } else {
    if (grepl("t-test", res$method)) {
      res <- res %>%
        mutate_at(vars(cohens_d), ~ formatC(., digits = 2, format = "f")) %>%
        mutate(df = if_else(grepl("Welch", method),
                            formatC(df, digits = 2, format = "f"),
                            formatC(df, digits = 0, format = "f"))) %>%
        mutate(p_value = report_p_value(p_value)) %>%
        select(df, estimate, p_value, cohens_d)

      output <- with(res,
           paste0("*t*(", res$df, ") = ", estimate, ", ", p_value, ", *d* = ",
                  cohens_d)
      )
    } else {
      if (grepl("Pearson", res$method)) {
        res <- res %>%
          mutate_at(vars(estimate), ~ formatC(., digits = 2, format = "f")) %>%
          mutate(p_value = report_p_value(p_value)) %>%
          select(df, estimate, p_value)

        output <- with(res, paste0("*r*(", res$df, ") = ", estimate, ", ", p_value))
      } else {
        if (grepl("Kendall", res$method)) {
          res <- res %>%
            mutate_at(vars(estimate), ~ formatC(., digits = 2, format = "f")) %>%
            mutate(p_value = report_p_value(p_value)) %>%
            select(df, estimate, p_value)

          output <- with(res, paste0("*r<sub>$latex\tau</sub>* = ", estimate, ", ", p_value))
        }
      }
    }


  }

  return(output)
}
