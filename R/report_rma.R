#' Report method for metafor's rma models
#'
#' Function to report a meta-analysis in APA style.
#'
#' @param results A tidy stats list.
#' @param identifier A character string identifying the model.
#' @param term A character string indicating which term you want to report the statistics of.
#' @param term_nr A number indicating which term you want to report the the statistics of.
#' @param statistic A character string of a statistic you want to extract from a model.
#'
#' @examples
#' # Read in a list of results
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' # Set the default results list
#' options(tidystats_list = results)
#'
#' # Example: regression term
#' # report("regression", term = "groupTrt")
#'
#' @import dplyr
#' @import stringr
#'
#' @export
report_rma <- function(results, identifier, term = NULL, term_nr = NULL, statistic = NULL) {

  # Extract the results of the specific model through its identifier
  res <- results[[identifier]]

  # Check whether the statistic exists, if provided
  if (!is.null(statistic)) {
    if (!statistic %in% res$statistic) {
      stop("Statistic not found.")
    }
  }

  # Check whether a term is provided, extract data if so, otherwise throw an error
  if (!is.null(term)) {
    res <- res[res$term == term, ]
  } else if (!is.null(term_nr)) {
    res <- res[res$term_nr == term_nr, ]
  } else {
    stop("No term provided")
  }

  # Check if only a single statistic is asked, otherwise produce a full line of APA results
  if (!is.null(statistic)) {
    output <- res$value[res$statistic == statistic]

    if (statistic == "p") {
      if (output < .001) {
        output <- "< .001"
      } else {
        output <- gsub(pattern = "0\\.", replacement = ".",
                       x = format(output, digits = 2, nsmall = 2))
      }
    } else {
      if (statistic != "df") {
        output <- format(output, digits = 2, nsmall = 2)
      }
    }
  } else {
    if (res$term[1] == "(Heterogeneity)") {
      # TODO
      output <- "TODO"
    } else if (res$term[1] == "(Moderators)") {
      # TODO
      output <- "TODO"
    } else {
      b <- format(res$value[res$statistic == "b"], digits = 2, nsmall = 2)
      SE <- format(res$value[res$statistic == "SE"], digits = 2, nsmall = 2)

      output <- paste0("*b* = ", b, ", *SE* = ", SE)

      if ("z" %in% res$statistic) {
        z <- format(res$value[res$statistic == "z"], digits = 2, nsmall = 2)
        output <- paste0(output, ", *z* = ", z)
      } else {
        t <- format(res$value[res$statistic == "t"], digits = 2, nsmall = 2)
        output <- paste0(output, ", *t* = ", t)
      }

      p = report_p_value(res$value[res$statistic == "p"])
      output <- paste0(output, ", ", p)

      res_CI <- filter(res, str_detect(statistic, "[0-9]+% CI"))

      CI_pct <- as.numeric(str_extract(res_CI$statistic, "[0-9]+"))
      CI_pct <- CI_pct[2] - CI_pct[1]

      CI_value1 <- format(res_CI$value[1], nsmall = 2, digits = 2)
      CI_value2 <- format(res_CI$value[2], nsmall = 2, digits = 2)

      CI <- paste0(CI_pct, "% CI ", "[", CI_value1, ", ", CI_value2, "]")
      output <- paste0(output, ", ", CI)
    }
  }

  return(output)
}

