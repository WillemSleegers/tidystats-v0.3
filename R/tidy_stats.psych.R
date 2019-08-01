#' tidy_stats method for psych's alpha objects
#'
#' Creates a tidystats data frame for a psych's alpha object.
#'
#' @param model An psych alpha object
#' @param args Unused.
#'
#' @examples
#' # Load packages
#' library(psych)
#' library(dplyr)
#'
#' # Create an empty list to store results in
#' results <- list()
#'
#' # Example: Cronbach's alpha
#' alpha_agreeableness <- bfi %>%
#'   select(A1, A2, A3, A4, A5) %>%
#'   alpha(check.keys = TRUE, warnings = FALSE)
#'
#' # Tidy stats
#' tidy_stats(alpha_agreeableness)
#'
#' # Example: Correlations
#' cors_agreeableness <- bfi %>%
#'   select(A1, A2, A3, A4, A5) %>%
#'   corr.test()
#'
#' # Tidy stats
#' tidy_stats(cors_agreeableness)
#'
#' @export
tidy_stats.psych <- function(model, args = NULL) {

  # Check the additional class to figure out what kind of analysis was performed
  if (class(model)[2] == 'alpha') {

    # Convert model output to a data frame
    output <- tibble::as_data_frame(model$total)

    # Add lower and upper limits of raw alpha
    output <- output %>%
      mutate(
        `raw alpha (lower)` = raw_alpha - 1.96 * ase,
        `raw alpha (upper)` = raw_alpha + 1.96 * ase
      )

    # Rename columns
    output <- rename_columns(output)

    # Tidy stats
    output <- output %>%
      dplyr::mutate(order = 1:dplyr::n()) %>%
      tidyr::gather("statistic", "value", -order) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-order)

    # Not included:
    # - Teliability if an item is dropped
    # - Item statistics
    # - Non missing response frequency for each item

    # Add method
    output <- dplyr::mutate(output, method = "alpha {psych}")
  } else if (class(model)[2] == "corr.test") {
    # Get call information
    call <- as.character(model$Call)
    names(call) <- names(model$Call)

    # Extract statistics
    # r
    model$r[upper.tri(model$r, diag = TRUE)] <- NA
    output <- model$r %>%
      tibble::as_tibble(rownames = "scale") %>%
      tidyr::gather("scale2", "r", -scale) %>%
      dplyr::filter(!is.na(r)) %>%
      tidyr::unite(term, scale2, scale, sep = " - ")

    # N
    if (length(model$n) == 1) {
      output <- dplyr::mutate(output, N = model$n)
    } else {
      model$n[upper.tri(model$n, diag = TRUE)] <- NA
      output <- model$n %>%
        tibble::as_tibble(rownames = "scale") %>%
        tidyr::gather("scale2", "N", -scale) %>%
        dplyr::filter(!is.na(N)) %>%
        tidyr::unite(term, scale2, scale, sep = " - ") %>%
        dplyr::full_join(output, by = "term")
    }

    # t
    model$t[upper.tri(model$t, diag = TRUE)] <- NA
    output <- model$t %>%
      tibble::as_tibble(rownames = "scale") %>%
      tidyr::gather("scale2", "t", -scale) %>%
      dplyr::filter(!is.na(t)) %>%
      tidyr::unite(term, scale2, scale, sep = " - ") %>%
      dplyr::full_join(output, by = "term")

    # p
    model$p[upper.tri(model$p, diag = TRUE)] <- NA
    output <- model$p %>%
      tibble::as_tibble(rownames = "scale") %>%
      tidyr::gather("scale2", "p", -scale) %>%
      dplyr::filter(!is.na(p)) %>%
      tidyr::unite(term, scale2, scale, sep = " - ") %>%
      dplyr::full_join(output, by = "term")

    # SE
    model$se[upper.tri(model$se, diag = TRUE)] <- NA
    output <- model$se %>%
      tibble::as_tibble(rownames = "scale") %>%
      tidyr::gather("scale2", "SE", -scale) %>%
      dplyr::filter(!is.na(SE)) %>%
      tidyr::unite(term, scale2, scale, sep = " - ") %>%
      dplyr::full_join(output, by = "term")

    # ci
    output <- model$ci %>%
      dplyr::select(lower, upper) %>%
      dplyr::bind_cols(output)

    if (is.na(call["alpha"])) {
      CI_level = .05
    } else {
      CI_level = as.numeric(call["alpha"])
    }

    names(output) <- stringr::str_replace(names(output), "lower",
      paste0(CI_level * 100 / 2, "% CI"))
    names(output) <- stringr::str_replace(names(output), "upper",
      paste0(100 - (CI_level * 100 / 2), "% CI"))

    # df
    output <- dplyr::mutate(output, df = N - 2)

    # Add term number
    output <- dplyr::mutate(output, term_nr = 1:nrow(output))

    # Make the data long
    output <- output %>%
      tidyr::gather("statistic", "value", -term, -term_nr)

    # Sort the statistics
    output <- output %>%
      dplyr::mutate(order = case_when(
        statistic == "r" ~ 1,
        statistic == "N" ~ 2,
        statistic == "SE" ~ 3,
        statistic == "t" ~ 4,
        statistic == "p" ~ 5
      )) %>%
      dplyr::arrange(term_nr, order) %>%
      dplyr::select(-order)

    # Add model information
    if (!is.na(call["method"])) {
      if (call["method"] == "spearman") {
        output <- dplyr::mutate(output,
          method = "Spearman's rank correlation rho {psych}")
      } else if (call["method"] == "kendall") {
        output <- dplyr::mutate(output,
          method = "Kendall's rank correlation tau {psych}")
      } else {
        output <- dplyr::mutate(output,
          method = "Pearson's product-moment correlation {psych}")
      }
    } else {
      output <- dplyr::mutate(output,
        method = "Pearson's product-moment correlation {psych}")
    }

    # Add notes
    if (model$adjust != "none") {
      output <- dplyr::mutate(output, notes = paste(model$adjust,
        "multiple test adjustment"))
    }

    output <- tibble::as_data_frame(output) %>%
      dplyr::select(term_nr, everything())
  } else if (class(model)[2] == 'ICC') {
    output <- model$results %>%
      tidyr::gather("statistic", "value", -type) %>%
      dplyr::arrange(type) %>%
      dplyr::rename(group = type)
  } else {
    stop("Models other than psych's alpha, ICC, and correlations are not yet
      supported.")
  }

  return(output)
}
