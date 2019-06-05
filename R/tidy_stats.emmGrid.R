#' Create a tidy stats data frame from an emmGrid object
#'
#' \code{tidy_stats.emmGrid} takes an emmGrid object and converts the object to a
#' tidy stats data frame.
#'
#' @param model Output of emmeans's \code{emmeans} without pairwise comparisons.
#'
#' @examples
#' # Load data
#' pigs <- emmeans::pigs
#'
#' # Conduct a linear regression
#' pigs.lm1 <- lm(log(conc) ~ source + factor(percent), data = pigs)
#'
#' # Tidy stats
#' tidy_stats(pigs.lm1)
#'
#' @export

tidy_stats.emmGrid <- function(model) {

  # Convert model output to a data frame
  output <- tibble::as_tibble(model)

  # Extract notes
  notes <- attributes(summary(model))$mesg

  # Figure out the confidence interval level
  CI_level <- notes[which(stringr::str_detect(notes, "Confidence level"))]
  CI_level <- readr::parse_number(CI_level)

  # Rename columns
  output <- rename_columns(output)

  # Rename confidence interval columns
  CI_columns <- c(
    "lower.CL" = paste0(CI_level * 100, "% CI lower"),
    "upper.CL" = paste0(CI_level * 100, "% CI upper")
  )
  colnames(output) <- dplyr::recode(colnames(output), UQS(CI_columns))

  # Combine the remaining columns into a term column
  terms <- attributes(summary(model))$pri.vars
  output <- unite(output, term, terms, sep = " - ")

  # Add term number
  output <- dplyr::mutate(output, term_nr = 1:n())

  # Tidy stats
  output <- output %>%
    tidyr::gather("statistic", "value", -term, -term_nr) %>%
    dplyr::arrange(term_nr)

  # Set the type of analysis
  output <- mutate(output, method = "Estimated marginal means {emmeans}")

  # Add notes
  notes <- paste(notes[1:length(notes)-1], collapse = '. ')
  if (notes != "") {
    output <- mutate(output, notes = notes)
  }

  # Reorder columns
  output <- dplyr::select(output, term_nr, everything())

  return(output)
}
