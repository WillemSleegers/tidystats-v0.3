#' Report method for an htest object
#'
#' Reports an htest object in APA style.

#' @examples
#' # Create empty results data frame
#' results <- new_stats_data_frame()
#' 
#' # Run a t-test
#' model <- t.test(1:10, y = c(7:20))
#' 
#' # Add model to results
#' add_stats(model, identifier = "M1", type = "hypothesis")
#'
#' # Report model
#' report(results, "M1")

#'@import dplyr

#'@export
report.htest <- function(results, identifier, statistic = NULL) {
    
    # Extract relevant statistics
    if (is.null(statistic)) {
        stats <- filter(results, identifier == identifier)
        
        text <- paste0(
            "*t*(", stats$parameter, ") = ", stats$statistic, ", ", 
            report_p_value(stats$p.value))
    } else {
        text <- paste(filter(results, identifier == identifier)$statistic)
    }
    
    return(text)
}

