#' Create a tidy stats data frame from an afex mixed object
#'
#' \code{tidy_stats.mixed} takes an afex mixed object and converts the object to
#' a tidy stats data frame.
#'
#' @param model Output of afex's \code{mixed}.
#'
#' @examples
#' # Load data
#' data("sk2011.2")
#' sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
#'
#' # Perform the analysis
#' sk_m1 <- mixed(response ~ instruction * inference * type +
#'   (inference * type | id), sk2_aff)
#'
#' # Tidy stats
#' tidy_stats(sk_m1) # Default ANOVA output
#' tidy_stats(sk_m1, args = list(summary = "lmer")) # lmer summary
#'
#' @export

tidy_stats.mixed <- function(model, args = NULL) {

  # Provide a message about the assumed summary function, if no function is
  # specified
  if (is.null(args)) {
    message("No summary function specified; assuming ANOVA")
    summary <- "ANOVA"
  } else {
    if (args$summary == "lmer") {
      summary <- "lmer"
    } else {
      summary <- "ANOVA"
    }
  }

  # Extract statistics
  if (summary == "lmer") {
    output <- tibble::as_tibble(summary(model)$coefficients, rownames = "term")
  } else {
    output <- tibble::as_tibble(model$anova_table, rownames = "term")
  }

  # Rename columns
  output <- rename_columns(output)

  # Add term number
  output <- dplyr::mutate(output, term_nr = 1:n())

  # Tidy stats
  output <- output %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::arrange(term_nr)

  # Set the type of analysis
  output <- mutate(output, method = "mixed model ANOVA {afex}")

  # Reorder columns
  output <- dplyr::select(output, term_nr, everything())

  return(output)
}
