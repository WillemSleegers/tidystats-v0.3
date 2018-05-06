#' Report descriptives helper functions
#'
#' @param identifier A character string identifying the descriptives.
#' @param var A character string identifying the exact variable, if needed.
#' @param group A character string identifiying the group, if needed.
#' @param results The tidystats results list.
#'
#' @examples
#' # Read in a list of results
#' descriptives <- read_stats(system.file("descriptives.csv", package = "tidystats"))
#'
#' options(tidystats_list = descriptives)
#'
#' # Report the mean
#' M("D4_avoidance")
#' M("D5_avoidance_anxiety", var = "avoidance")
#'
#' # Report the standard deviation
#' SD("D4_avoidance")
#'
#' @name report_descriptives
#'
#' @export
M <- function(identifier, var = NULL, group = NULL, results = getOption("tidystats_list")) {
  report(identifier = identifier, var = var, group = group, results = results, statistic = "M")
}

#' @name report_descriptives
#'
#' @export
SD <- function(identifier, var = NULL, group = NULL, results = getOption("tidystats_list")) {
  report(identifier = identifier, var = var, group = group, results = results, statistic = "SD")
}
