#' Tidy correlation
#'
#' Create tidy correlation output.

#' @examples
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#' 
#' model <- cor.test(x, y)
#' correlation("M1", "hypothesis test", model)

#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

#'@export
correlation <- function(identifier, type, model, description = NULL) {
    
    # Use the broom's tidy() function to retrieve relevant statistics, add the identifier and type information, and store the results in a data frame
    tidy(model) %>%
        select(estimate, statistic, p.value, parameter) %>%
        mutate(
            identifier = identifier,
            type = type
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