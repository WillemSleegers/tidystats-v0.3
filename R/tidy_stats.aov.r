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
    method = rep("ANOVA", length(attr(model$terms, "dataClasses")))
  )

  # Add term(s)
  output$term <- c(attr(model$terms, "term.labels"), "Residuals")

  # Add sums of squares, mean squares, F statistic, degrees of freedom, and p value
  output <- bind_cols(output, as_data_frame(anova(model)))

  # Rename variables
  output <- rename(output,
                   sum_squares = `Sum Sq`,
                   mean_squares = `Mean Sq`,
                   statistic = `F value`,
                   df = Df,
                   p_value = `Pr(>F)`)

  # Reorder variables
  output <- select(output, method, term, sum_squares, mean_squares, statistic, df, p_value)

  return(output)
}
