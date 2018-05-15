#' tidy_stats method for psych's alpha objects
#'
#' Creates a tidystats data frame for a psych's alpha object.
#'
#' @param model An psych alpha object
#'
#' @examples
#' library(psych)
#' results <- list()
#'
#' bfi %>%
#'   select(A1, A2, A3, A4, A5) %>%
#'   alpha(check.keys = TRUE) %>%
#'   add_stats(results, identifier = 'alpha_agreeableness')
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
tidy_stats.psych <- function(model) {

  # Check if it is from the pysch package
  if (class(model)[2] != 'alpha') {
    stop("Models other than psych's alpha are not yet supported.")
  }

  # Extract statistics
  output <- as_data_frame(model$total) %>%
    rename(
      raw_alpha = raw_alpha,
      std_alpha = std.alpha,
      G6 = `G6(smc)`,
      signal_noise_ratio = `S/N`,
      alpha_standard_error = ase,
      M = mean,
      SD = sd
    ) %>%
    mutate(
      order = 1:n(),
      raw_alpha_lower = raw_alpha - 1.96 * alpha_standard_error,
      raw_alpha_upper = raw_alpha + 1.96 * alpha_standard_error
      ) %>%
    gather("statistic", "value", -order) %>%
    arrange(order) %>%
    select(-order)
  # Not included:
  # - reliability if an item is dropped
  # - item statistics
  # - Non missing response frequency for each item

  # Add method
  output$method <- "alpha {psych}"

  return(output)
}
