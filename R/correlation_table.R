#' Create a correlation table
#'
#' Function to create a correlation table.
#'
#' @param correlations A data frame or matrix containing the correlations.
#' @param triangle A character string indicating whether the lower, upper, or both halves should be visible, must be one of 'lower', 'upper', or 'both'. You can specify just the initial letter.
#' @param p_values A data frame or matrix containing the p-values.
#' @param diagonal A vector of values to be placed on the diagonal. Default is a vector of 1s.
#' @param labels A vector of strings containing the labels for each variable.
#' @param digits An integer indicating the number of decimal places.
#'
#' @examples
#' # Add example
#'
#' @import knitr
#'
#' @export

correlation_table <- function(correlations, triangle = "lower", p_values = NULL, diagonal = NULL,
                              labels = NULL, digits = 2) {

  # Check whether correlations are provided
  if (is.null(correlations)) {
    stop("No correlations provided.")
  }

  # Convert the output to a data frame and make each column a character
  correlations <- as.data.frame(correlations) %>%
    mutate_all(prettyNum, digits = digits)

  # Add significance asterisks
  output <- correlations

  # Set NA values to 1
  diag(p_values) <- 1

  # Add asterisks
  output[p_values < 0.05] <- paste0(correlations[p_values < 0.05], "*")
  output[p_values < 0.01] <- paste0(correlations[p_values < 0.01], "**")
  output[p_values < 0.001] <- paste0(correlations[p_values < 0.001], "***")

  # Get the kind of triangle
  triangle <- match.arg(triangle, choices = c("lower", "upper", "both"))

  if (triangle == "lower") {
    output[upper.tri(output)] <- NA
  } else {
    output[lower.tri(output)] <- NA
  }

  # Add a column with the variable names
  output$var <- names(output)

  # Move var to the beginning of the data frame
  output <- select(output, var, everything())

  # Create table
  output <- kable(output, caption = paste("Correlation matrix"))

  return(output)
}
