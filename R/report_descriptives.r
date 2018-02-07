#' Report descriptives
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
