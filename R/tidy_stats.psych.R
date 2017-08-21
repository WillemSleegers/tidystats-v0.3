#' tidy_stats method for psych's alpha objects
#'
#' Creates a tidystats data frame for a psych's alpha object.
#'
#' @param model An psych alpha object
#'
#' @example
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
  # Not included: Reliability if an item is dropped and item statistics
  output <- as_data_frame(model[[1]]) %>%
    rename(
      cronbachs_alpha = raw_alpha,
      std_alpha = std.alpha,
      guttmans_lambda6 = `G6(smc)`,
      signal_noise = `S/N`
    ) %>%
    mutate(
      order = 1:n()
      ) %>%
    gather("statistic", "value", -order) %>%
    arrange(order) %>%
    select(-order)

  # Add method
  output$method <- "alpha"

  return(output)
}
