#' Read a .csv file that was produced with \code{write_stats}
#'
#' \code{read_stats} can convert a .csv file containing the statistical results that was produced with \code{write_stats}. It returns a list containing the results, with the identifier as the name for each list element.
#'
#' @param file Path to the tidy stats data file
#'
#' @examples
#' results <- read_stats(system.file("results.csv", package = "tidystats"))
#'
#' @import dplyr
#' @import readr
#' @import purrr
#' @importFrom magrittr "%>%"
#'
#' @export

read_stats <- function(file) {
  col_types <- cols(
    identifier = col_character(),
    group = col_character(),
    term_nr = col_double(),
    term = col_character(),
    statistic = col_character(),
    value = col_double(),
    method = col_character(),
    notes = col_character()
  )

  readr::read_csv(file, col_types = col_types) %>%
    split(., .$identifier) %>%

    # remove superfluous identifier column and empty columns
    purrr::map( ~dplyr::select(.x, -identifier) %>%
                  dplyr::select_if(~!all(is.na(.))))
}
