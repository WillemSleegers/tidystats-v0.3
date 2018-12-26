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

  # Check the additional class
  if (class(model)[2] == 'alpha') {
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
  } else if (class(model)[2] == "corr.test") {
    # Get call information
    call <- as.character(model$Call)
    names(call) <- names(model$Call)

    # Extract statistics
    # r
    model$r[upper.tri(model$r, diag = TRUE)] <- NA
    output <- model$r %>%
      as_tibble(rownames = "scale") %>%
      gather("scale2", "r", -scale) %>%
      filter(!is.na(r)) %>%
      unite(term, scale2, scale, sep = " - ")

    # N
    if (length(model$n) == 1) {
      output <- mutate(output, N = model$n)
    } else {
      model$n[upper.tri(model$n, diag = TRUE)] <- NA
      output <- model$n %>%
        as_tibble(rownames = "scale") %>%
        gather("scale2", "N", -scale) %>%
        filter(!is.na(N)) %>%
        unite(term, scale2, scale, sep = " - ") %>%
        full_join(output, by = "term")
    }

    # t
    model$t[upper.tri(model$t, diag = TRUE)] <- NA
    output <- model$t %>%
      as_tibble(rownames = "scale") %>%
      gather("scale2", "t", -scale) %>%
      filter(!is.na(t)) %>%
      unite(term, scale2, scale, sep = " - ") %>%
      full_join(output, by = "term")

    # p
    model$p[upper.tri(model$p, diag = TRUE)] <- NA
    output <- model$p %>%
      as_tibble(rownames = "scale") %>%
      gather("scale2", "p", -scale) %>%
      filter(!is.na(p)) %>%
      unite(term, scale2, scale, sep = " - ") %>%
      full_join(output, by = "term")

    # SE
    model$se[upper.tri(model$se, diag = TRUE)] <- NA
    output <- model$se %>%
      as_tibble(rownames = "scale") %>%
      gather("scale2", "SE", -scale) %>%
      filter(!is.na(SE)) %>%
      unite(term, scale2, scale, sep = " - ") %>%
      full_join(output, by = "term")

    # ci
    output <- model$ci %>%
      select(lower, upper) %>%
      bind_cols(output)

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
    output <- mutate(output, df = N - 2)

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
      arrange(term_nr, order) %>%
      select(-order)

    # Add model information
    if (!is.na(call["method"])) {
      if (call["method"] == "spearman") {
        output$method <- "Spearman's rank correlation rho {psych}"
      } else if (call["method"] == "kendall") {
        output$method <- "Kendall's rank correlation tau {psych}"
      } else {
        output$method <- "Pearson's product-moment correlation {psych}"
      }
    } else {
      output$method <- "Pearson's product-moment correlation {psych}"
    }

    # Add notes
    if (model$adjust != "none") {
      output$notes <- paste(model$adjust, "multiple test adjustment")
    }

    output <- as_data_frame(output) %>%
      select(term_nr, everything())
  } else if (class(model)[2] == 'ICC') {
    output <- model$results %>%
      gather("statistic", "value", -type) %>%
      arrange(type) %>%
      rename(group = type)
  } else {
    stop("Models other than psych's alpha, ICC, and correlations are not yet
      supported.")
  }

  return(output)
}
