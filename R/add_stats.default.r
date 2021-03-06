#' add_stats default function
#'
#' \code{add_stats.default} is the default add_stats function, which takes the
#' output of a statistical test, tidies the output, and adds it to a tidystats
#' list.
#'
#' @param results A tidy stats list.
#' @param output Output of a statistical test.
#' @param identifier A character string identifying the model. Automatically
#' created if not provided.
#' @param type A character string indicating the type of test. One of
#' "hypothesis", "manipulation check", "contrast", "descriptives", or "other".
#' Can be abbreviated.
#' @param confirmatory A boolean to indicate whether the statistical test was
#' confirmatory (TRUE) or exploratory (FALSE). Can be NA.
#' @param notes A character string to add additional information. Some
#' statistical tests produce notes information, which will be overwritten if
#' notes are provided.
#' @param class A character string to indicate which function was used to
#' produce the output. See 'Details' for a list of supported functions.
#' @param args An optional list of additional arguments. Can be used to specify
#' how model results should be summarized.
#'
#'
#' @examples
#' # Create an empty list to store the results in
#' results <- list()
#'
#' # Example: t-test
#' model_t_test <- t.test(extra ~ group, data = sleep)
#' results <- add_stats(results, model_t_test, identifier = "t_test")
#'
#' # Example: correlation
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#' model_correlation <- cor.test(x, y)
#'
#' # Add output to the results list, only storing the correlation and p-value
#' results <- add_stats(results, model_correlation, identifier = "correlation")
#'
#' # Example: Regression
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#'
#' model_lm <- lm(weight ~ group)
#'
#' # Add output to the results list, with notes
#' results <- add_stats(results, model_lm, identifier = "regression", notes =
#'   "regression example")
#'
#' # Example: ANOVA
#' model_aov <- aov(yield ~ block + N * P * K, npk)
#'
#' results <- add_stats(results, model_aov, identifier = "ANOVA")
#'
#' # Example: Within-Subjects ANOVA
#' model_aov_within <- aov(extra ~ group + Error(ID/group), data = sleep)
#'
#' results <- add_stats(results, model_aov_within, identifier = "ANOVA_within")
#'
#' @export

add_stats.default <- function(results, output, identifier = NULL, type = NULL,
  confirmatory = NULL, notes = NULL, class = NULL, args = NULL) {

  # Create an identifier if it is not specified, else check whether it already
  # exists
  if (is.null(identifier)) {

    if (deparse(substitute(output)) == ".") {
      identifier <- paste0("M", formatC(length(results)+1, width = "1",
        format = "d"))
    } else {
      identifier <- deparse(substitute(output))
    }

  } else {
    if (!is.null(names(results))) {
      if (identifier %in% names(results)) {
        stop("Identifier already exists.")
      }
    }
  }

  # Create the new element
  new_element <- tidy_stats(output)

  # Add the type
  if (!is.null(type)) {
    type <- match.arg(type, choices = c("hypothesis", "manipulation check",
      "contrast", "descriptives", "other"))

    new_element <- dplyr::mutate(new_element, type = dplyr::case_when(
      substr(type, 1, 1) == "h" ~ "hypothesis",
      substr(type, 1, 1) == "m" ~ "manipulation check",
      substr(type, 1, 1) == "c" ~ "contrast",
      substr(type, 1, 1) == "d" ~ "descriptives",
      TRUE ~ NA_character_)
    )
  }

  # Add information whether the analysis was confirmatory or not
  if (!is.null(confirmatory)) {
    new_element <- mutate(new_element, confirmatory = dplyr::case_when(
      confirmatory == TRUE ~ TRUE,
      confirmatory == FALSE ~ FALSE,
      TRUE ~ NA)
    )
  }

  # Add notes
  if (!is.null(notes)) {
    new_element$notes <- notes
  }

  # Add the new element to the list
  results[[identifier]] <- new_element

  # Return the new results list
  return(results)
}
