#' Create a tidy stats data frame from an htest object
#'
#' \code{tidy_stats.htest} takes an htest object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{t.test()}.
#'
#' @examples
#' # Conduct a t-test
#' model_t_test <- t.test(extra ~ group, data = sleep)
#' tidy_stats(model_t_test)
#'
#' # Conduct a correlation
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#' model_correlation <- cor.test(x, y)
#' tidy_stats(model_correlation)
#'
#' # Conduct a chi-square test
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#'
#' model_chi_square <- chisq.test(M)
#' tidy_stats(model_chi_square)
#'
#' @export
tidy_stats.htest <- function(model) {

  # Extract statistics
  output <- dplyr::bind_rows(
    if (!is.null(model$estimate)) {
      test_statistic <- names(model$estimate)

      test_statistic <- ifelse(test_statistic == "odds ratio", "OR",
        test_statistic)

      tibble::data_frame(statistic = test_statistic,
        value = model$estimate)
    },
    if (!is.null(model$statistic)) {
      tibble::data_frame(statistic = names(model$statistic),
        value = model$statistic)
    },
    if (!is.null(model$parameter)) {
      tibble::data_frame(statistic = names(model$parameter),
        value = model$parameter)
    },
    tibble::data_frame(statistic = "p", value = model$p.value),
    if (!is.null(model$conf.int)) {
      level <- attr(model$conf.int, "conf.level")*100
      tibble::data_frame(
        statistic = c(paste0(level, "% CI lower"), paste0(level, "% CI upper")),
        value = c(model$conf.int[1], model$conf.int[2])
      )
    },
    if (!is.null(model$null.value)) {
      tibble::data_frame(statistic = "null value", value = model$null.value)
    }
  )

  # Add the method
  # (use trimws to remove the leading space from a Two Sample t-test)
  output$method <- trimws(model$method)

  # Rename the 'cor' statistic to 'r' when the method is a Pearson correlation
  if (stringr::str_detect(output$method[1], "Pearson")) {
    output$statistic[output$statistic == "cor"] <- "r"
  }

  # Add additional information
  if (!is.null(model$alternative)) {
    output$notes <- paste("alternative hypothesis:", model$alternative)
  }

  if (stringr::str_detect(model$method, "simulated p-value")) {
    output$notes <- paste0(output$notes, "; ", "simulated p-value based on ",
      stringr::str_extract(model$method, "[0-9](e\\+)?([0-9].)?"),
      " replicates")
    output$method <- "Fisher's Exact Test for Count Data"
  } else if (stringr::str_detect(model$method, "hybrid")) {
    output$notes <- paste0(output$notes, "; ", "hybrid using asym.chisq. iff ",
      stringr::str_extract(model$method, "(?<=\\()(.*)(?=\\))"))
    output$method <- "Fisher's Exact Test for Count Data"
  }

  return(output)
}




