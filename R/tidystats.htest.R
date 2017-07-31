#' Tidystats method for an htest object
#'
#' Creates a tidystats data.frame for an htest object. 

#' @Dependencies
#' broom

#' @examples
#' ttest <- t.test(1:10, y = c(7:20))
#' #' tidystats.htest("ttest_x_y", ttest, "A t-test for x and y")

tidystats.htest <- function(identifier, model, description = NULL) {
    # Tidy the result to a data.frame
    broom::tidy(model) %>% 
        select(estimate, statistic, p.value, parameter) %>% 
        mutate(
            identifier = identifier,
            type = "t-test"
        ) %>% 
        select(identifier, type, everything()) -> output
    
    # Add description if provided
    if (!is.null(description)) {
        output %>%
            mutate(
                description = description
            ) -> output
    }
    
    return(output)
}
