#' Create a tidy stats data frame from an aovlist object
#'
#' \code{tidy_stats.aovlist} takes an aovlist object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{aov()} including within-subject factors.
#'
#' @examples
#' # Conduct a within-subjects ANOVA
#' model_aov_within <- aov(extra ~ group + Error(ID/group), data = sleep)
#' tidy_stats(model_aov_within)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom purrr map2_df
#'
#' @export

tidy_stats.aovlist <- function(model) {

  # Custom function to extract stats with purrr:map2_df()
  extract_stats <- function(x, y) {
    df <- tibble::as_data_frame(summary(x)[[1]])

    if (nrow(df) > 0) {
      df$term <- rownames(summary(x)[[1]])
    }

    return(df)
  }

  # Extract statistics
  output <- model %>%
    purrr::map2_df(names(model), extract_stats) %>%
    dplyr::rename(
      df = Df,
      SS = `Sum Sq`,
      MS = `Mean Sq`,
      `F` = `F value`,
      p = `Pr(>F)`
    ) %>%
    dplyr::mutate(term_nr = 1:n()) %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(term_nr)

  # Remove spaces from the term variable
  output$term <- gsub(" ", "", output$term)

  # Get classes of the predictors
  classes <- unlist(lapply(stats::model.frame(model), class))[-1]

  # Add kind of ANOVA
  output <- dplyr::mutate(output, method = dplyr::case_when(
    "numeric" %in% classes ~ "ANCOVA",
    !grepl("Residuals", output$term[1]) ~ "Mixed ANOVA",
    sum(!is.na(classes)) == 2 ~ "One-way repeated measures ANOVA",
    sum(!is.na(classes)) == 3 ~ "Factorial repeated measures ANOVA",
    TRUE ~ "Repeated measures ANOVA"
  ))

  # Remove between subject residuals if there are no between subject effects
  if (!output$method[1] %in% c("ANCOVA", "Mixed ANOVA")) {
    output <- output[-1:-3, ]
  }

  # Change term names
  # TODO: Get rid of the for loop and use a vectorized solution?
  for (i in 1:nrow(output)) {
    if (output$term[i] == "Residuals") {
      output$term[i] <- paste(term, "Residuals", sep = "_")
    } else {
      term <- output$term[i]
    }
  }

  # Reorder columns
  output <- dplyr::select(output, term_nr, everything())

  return(output)
}
