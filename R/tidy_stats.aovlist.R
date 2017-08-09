#' tidy_stats method for an aovlist object
#'
#' Creates a tidystats data frame for an aovlist object.
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
    df <- as_data_frame(summary(x)[[1]])

    if (nrow(df) > 0) {
      df$error_term <- y
      df$term <- rownames(summary(x)[[1]])
    }

    return(df)
  }

  # Convert stats to data frame
  # and repeat df_error across each term in a new variable
  model %>%
    map2_df(names(model), extract_stats) %>%
    group_by(error_term) %>%
    mutate(df_error = last(Df)) -> output

  # Remove spaces from the term variable
  output$term <- gsub(output$term, pattern = " ", replacement = "")

  # Remove rows of the residuals
  output <- filter(output, term != "Residuals")

  # Get classes of each variable
  classes <- as_data_frame(lapply(model.frame(model), class))

  # Merge with output
  classes %>%
    gather("term", "class") %>%
    right_join(output, classes, by = "term") -> output

  # Add kind of ANOVA
  output <- mutate(output, method = case_when(
    "numeric" %in% class ~ "ANCOVA",
    sum(!grepl(":", error_term)) > 0 ~ "Mixed ANOVA",
    sum(!is.na(output$class)) == 1 ~ "One-way repeated measures ANOVA",
    sum(!is.na(output$class)) == 2 ~ "Factorial repeated measures ANOVA",
    TRUE ~ "Repeated measures ANOVA"
  ))

  # Rename variables
  output <- rename(output,
                   df_model = Df,
                   sum_squares = `Sum Sq`,
                   mean_squares = `Mean Sq`,
                   statistic = `F value`,
                   p_value = `Pr(>F)`)

  # Select and order variables
  output <- select(output, method, term, sum_squares, mean_squares, statistic,
                   df_model, df_error, p_value)

  return(output)
}
