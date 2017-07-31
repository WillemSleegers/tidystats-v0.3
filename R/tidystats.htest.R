#' Title
#'
#' Description

#' @Dependencies
#' broom

#' @examples
#' tidystats.t.test()

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
