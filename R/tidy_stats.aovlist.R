#' Create a tidy stats data frame from an aovlist object
#'
#' \code{tidy_stats.aovlist} takes an aovlist object and converts the object to a tidy stats data frame.
#'
#' @param model Output of \code{aov()} including within-subject factors.
#'
#' @examples
#' # Conduct a within-subjects ANOVA
#' model_aov_within <- aov(extra ~ group + Error(ID/group), data = sleep)
#' tidy_stats(model_aov_within)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom purrr map2_df
#'
#' @export

tidy_stats.aovlist <- function(model) {

  # Extract statistics
  output <- model %>%
    summary() %>%
    as.list.data.frame() %>%
    purrr::map(as.list.data.frame) %>%
    purrr::map(as.data.frame) %>%
    purrr::map(rownames_to_column, "term") %>%
    purrr::map_df(as_tibble, .id = "group") %>%
    dplyr::rename(
      df = Df,
      SS = `Sum.Sq`,
      MS = `Mean.Sq`,
      `F` = `F.value`,
      p = `Pr..F.`
    ) %>%
    dplyr::mutate(term_nr = 1:n()) %>%
    tidyr::gather("statistic", "value", -group, -term, -term_nr) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(term_nr)

  # Remove spaces from the term variable
  output$term <- stringr::str_replace_all(output$term, " ", "")

  # Add a group column
  output <- dplyr::mutate(output, group = stringr::str_replace(group, "Error: ", ""))

  # Get classes of the predictors
  classes <- unlist(lapply(stats::model.frame(model), class))[-1]

  # Add kind of ANOVA
  output <- dplyr::mutate(output, method = dplyr::case_when(
    "numeric" %in% classes ~ "ANCOVA",
    !stringr::str_detect(first(dplyr::pull(output, term)), "Residuals") ~ "Mixed ANOVA",
    sum(!is.na(classes)) == 2 ~ "One-way repeated measures ANOVA",
    sum(!is.na(classes)) == 3 ~ "Factorial repeated measures ANOVA",
    TRUE ~ "Repeated measures ANOVA"
  ))

  # Reorder columns
  output <- dplyr::select(output, group, term_nr, everything())

  return(output)
}
