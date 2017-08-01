#' Report generic function
#'
#' \code{report} is a generic function that reports a test in APA style.

# @examples
#

#'@export
report <- function(results, identifier, term = NULL, statistic = NULL) {
    # Find out which test was used
    testmethod <- filter(results, identifier == identifier) %>%
        pull(method)
    
    # Run the appropriate report function
    if (testmethod == "Welch Two Sample t-test") {
        report_t_test(results, identifier, statistic)
    }
}
