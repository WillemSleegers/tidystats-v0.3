#' Report descriptives
#'
#' \code{D} is a general function that returns a single descriptives from a tidystats list.
#'
#' @param results A tidystats list.
#' @param identifier An identifier.
#' @param var A character string indicating which variable you want to report the descriptive of.
#' @param group (Optional) A character string indicating which group you want to report the descriptive of.
#' @param statistic A character string indicating which statistic you want to report.
#'
#' @details \code{D} retrieves the descriptive of a specific variable (and group).
#'
#' @examples
#' # Read in a list of results
#' descriptives <- read_stats(system.file("descriptives.csv", package = "tidystats"))
#'
#' # Generic example:
#' D(descriptives, identifier = "D1", var = "avoidance", statistic = "M")
#'
#' # Shorter examples:
#' # Mean
#' M(descriptives, "D1", "avoidance")
#'
#' # SD
#' SD(descriptives, "D1", "avoidance")
#'
#' @import dplyr
#'
#' @export
D <- function(list, identifier, var, group = NULL, statistic = NULL) {

  # Check whether the identifier exists
  if (!identifier %in% names(list)) {
    stop("Identifier not found.")
  }

  # Create identification vars
  var_res <- var
  group_res <- group
  statistic_res <- statistic

  # Extract the data frame from the list
  df <- magrittr::extract2(list, identifier)

  # Extract the specific descriptive to be reported
  if ("group" %in% names(df)) {
    output <- filter(df,
                     var == var_res,
                     group == group_res,
                     statistic_res == statistic)
  } else {
    output <- filter(df,
                     var == var_res,
                     statistic_res == statistic)
  }

  output <- pull(output, value)

  return(output)
}

#' @rdname D
#' @export
M <- function(list, identifier, var, group = NULL) {
  D(list = list, identifier = identifier, var = var, group = group, statistic = "M")
}

#' @rdname D
#' @export
SD <- function(list, identifier, var, group = NULL) {
  D(list = list, identifier = identifier, var = var, group = group, statistic = "SD")
}
