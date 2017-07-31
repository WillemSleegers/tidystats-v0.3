#' Tidystats method for an htest object
#'
#' Creates a tidystats data.frame for an htest object.

#' @examples
#' ttest <- t.test(1:10, y = c(7:20))
#' tidystats.htest("ttest_x_y", "Hypothesis", ttest, "A t-test for x and y")
#'
#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

tidystats.htest <- function(identifier, type = "", model, description = NULL) {

    # Tidy the result to a data.frame
    broom::tidy(model) %>%
        select(estimate, statistic, p.value, parameter, method) %>%
        mutate(
            identifier = identifier,
            type = type
        ) %>%
        select(identifier, type, method, everything()) -> output

    # Add description if provided
    if (!is.null(description)) {
        output %>%
            mutate(
                description = description
            ) -> output
    }

    return(output)
}
