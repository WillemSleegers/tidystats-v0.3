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
M <- function(identifier, var = NULL, group = NULL, results = getOption("tidystats_list")) {
  report(identifier = identifier, var = var, group = group, results = results, statistic = "M")
}

#' @export
SD <- function(identifier, var = NULL, group = NULL, results = getOption("tidystats_list")) {
  report(identifier = identifier, var = var, group = group, results = results, statistic = "SD")
}
