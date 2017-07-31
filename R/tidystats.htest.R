#' Tidystats method for an htest object
#'
#' Creates a tidystats data.frame for an htest object. 

#' @Dependencies
#' broom

#' @examples
#' ttest <- t.test(1:10, y = c(7:20))
#' tidystats.htest(ttest)

tidystats.htest <- function(ttest, description = "") {
    # Tidy the result to a data.frame
    results <- broom::tidy(ttest)
    
    # Create variables
    identifier <- ""
    type <- "t.test"
    term <- "t"
    estimate <- results$estimate
    std_error <- NA
    statistic <- results$statistic
    p_value <- results$p.value
    
    # Return data.frame of all variables
    data.frame(identifier, type, term, estimate, std_error, statistic, p_value, description)
}
