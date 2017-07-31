#' Addstats generic function
#'
#' \code{addstats} is a generic function to add a line of tidy stats to a tidy stats data.frame.

#' @examples
#' results <- new_stats_data_frame()
#' ttest <- t.test(1:10, y = c(7:20))
#' results <- addstats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y")

#'@import dplyr

addstats <- function(results, model, identifier, type = "", description = NULL) {
    
    # Create the new row
    newrow <- tidystats(model, identifier, type, description) %>% 
        mutate_if(is.factor, as.character)
    
    # Check if the identifier already exists
    if(identifier %in% results$identifier) {
        stop("Identifier already exists.")
    }
    
    # Add the new row to the data.frame
    if (is.na(results[1, 1])) {
        # Overwrite the existing result
        newresults <- newrow
    } else {
        newresults <- bind_rows(results, newrow)
    }
    
    # Return the new results data.frame
    return(newresults)
}
