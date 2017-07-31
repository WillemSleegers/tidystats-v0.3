#' Tidystats method for an aov object
#'
#' Creates a tidystats data.frame for an aov object.

#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

#'@export
tidystats.aov <- function(model, identifier, type = "", description = NULL) {
    
    # Tidy the result to a data.frame
    tidy(model) %>%
        rename(
            parameter = df,
        )
        mutate(
            identifier = identifier,
            type = type,
            method = "ANOVA"
        ) %>%
        select(identifier, type, method, term, everything()) -> output
    
    # Add description if provided
    if (!is.null(description)) {
        output %>%
            mutate(
                description = description
            ) -> output
    }
    
    return(output)
}
