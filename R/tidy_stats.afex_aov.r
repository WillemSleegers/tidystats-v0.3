#' Create a tidy stats data frame from an afex aov object
#'
#' \code{tidy_stats.aov} takes an afex aov object and converts the object to a
#' tidy stats data frame.
#'
#' @param model Output of afex's \code{aov_ez}, \code{aov_car}, or \code{aov_4}
#'
#' @examples
#' # Load data
#' data(obk.long, package = "afex")
#'
#' # Conduct an ANOVA
#' afex_aov <- aov_4(value ~ treatment * gender + (phase * hour | id),
#'   data = obk.long, observed = "gender")
#'
#' # Tidy stats
#' tidy_stats(afex_aov)
#'
#' @export

tidy_stats.afex_aov <- function(model) {

  # Convert model output to a data frame
  output <- tibble::as_tibble(model$anova_table, rownames = "term")

  # Rename columns
  output <- rename_columns(output)

  # Add term number
  output <- dplyr::mutate(output, term_nr = 1:n())

  # Tidy stats
  output <- output %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::arrange(term_nr)

  # Set the type of analysis
  output <- mutate(output, method = "ANOVA {afex}")

  # Reorder columns
  output <- dplyr::select(output, term_nr, everything())

  return(output)
}
