#' Report generic function
#'
#' \code{report} is a generic function that reports a test in APA style.

# @examples
#

#'@export
report <- function(results, identifier, term = NULL, statistic = NULL) {
    # Find out which test was used
    method <- filter(results, identifier == identifier) %>%
        pull(method)

    # Run the appropriate report function
    output <- case_when(
        grepl("t-test", method) ~ report_t_test(results, identifier, statistic)
    )
    
    return(output)
}
