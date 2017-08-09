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
    output <- pull(res[statistic])

    if (grepl("df", statistic)) {
      output <- formatC(output, digits = 0, format = "d")
    } else {
      output <- formatC(output, digits = 2, format = "f")
    }
  } else {
    if (term == "(Model)") {
      res <- res %>%
        mutate(
          adj_r_squared = formatC(adj_r_squared, digits = 2, format = "f"),
          statistic = formatC(statistic, digits = 2, format = "f")) %>%
        mutate(
          df_model = formatC(df_model, digits = 0, format = "f"),
          df_error = formatC(df_error, digits = 0, format = "f")) %>%
        mutate(p_value = report_p_value(p_value))

      output <- with(res,
                     paste0("adjusted *R*<sup>2</sup> = ", adj_r_squared, ", *F*(", df_model, ", ",
                            df_error, ") = ", statistic, ", ", p_value)
      )
    } else {
      res <- res %>%
        mutate_at(
          vars(estimate, std_error, statistic),
          funs(formatC(., digits = 2, format = "f"))) %>%
        mutate(df_error = formatC(df_error, digits = 0, format = "f")) %>%
        mutate(p_value = report_p_value(p_value))

      output <- with(res,
                     paste0("*b* = ", estimate, ", *SE* = ", std_error,
                            ", *t*(",  df_error, ") = ", statistic, ", ", p_value)
      )
    }

  }

  return(output)
}
