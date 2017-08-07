#' tidy_stats method for an aov object
#'
#' Creates a tidystats data frame for an aov object.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.aov <- function(model) {

  # Create tidy stats data frame
  output <- data_frame(
    term = labels(model$terms)
  )

  # Add sums of squares, mean squares, F statistic, degrees of freedom, and p value of the terms
  output <- bind_cols(output, head(as_data_frame(as_data_frame(summary(model)[[1]])), nrow(output)))

  # Add degrees of freedom of the residuals
  output$df_error <- model$df.residual

  # Determine the type of ANOVA
  classes <- attr(model$terms, "dataClasses")[-1]
  method <- case_when(
    sum(classes == "numeric") > 0 ~ "ANCOVA",
    sum(classes == "factor") == 1 ~ "One-way ANOVA",
    sum(classes == "factor") == 2 ~ "Factorial ANOVA",
    TRUE ~ "ANOVA"
  )

  # Rename variables
  output <- rename(output,
                   sum_squares = `Sum Sq`,
                   mean_squares = `Mean Sq`,
                   statistic = `F value`,
                   df_model = Df,
                   p_value = `Pr(>F)`)

  # Reorder variables
  output <- select(output, method, term, sum_squares, mean_squares, statistic, df_model, df_error, p_value)

  return(output)
}
