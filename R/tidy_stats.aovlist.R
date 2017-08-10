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
      df$term <- paste(y, rownames(summary(x)[[1]]), sep = "-")
    }

    return(df)
  }

  # Extract statistics
  output <- model %>%
    map2_df(names(model), extract_stats) %>%
    rename(
      df = Df,
      SS = `Sum Sq`,
      MS = `Mean Sq`,
      `F` = `F value`,
      p = `Pr(>F)`
    ) %>%
    mutate(order = 1:n()) %>%
    gather("statistic", "value", -term, -order) %>%
    filter(!is.na(value)) %>%
    arrange(order) %>%
    select(-order)

  # Remove spaces from the term variable
  output$term <- gsub(" ", "", output$term)

  # Get classes of each variable
  classes <- unlist(lapply(model.frame(model), class))[-1]

  # Add kind of ANOVA
  output <- mutate(output, method = case_when(
    "numeric" %in% classes ~ "ANCOVA",
    !grepl("Residuals", output$term[1]) ~ "Mixed ANOVA",
    sum(!is.na(classes)) == 2 ~ "One-way repeated measures ANOVA",
    sum(!is.na(classes)) == 3 ~ "Factorial repeated measures ANOVA",
    TRUE ~ "Repeated measures ANOVA"
  ))

  return(output)
}
