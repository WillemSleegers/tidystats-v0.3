#' Report method for a t-test.
#'
#' Function to report a t-test in APA style.

# @examples
# ttest <- t.test(1:10, y = c(7:20))
# tidystats.htest("ttest_x_y", "Hypothesis", ttest, "A t-test for x and y")
#'
#'@import dplyr
#'@import broom
#'@import tibble
#'@importFrom magrittr %>%

#'@export
report_t_test <- function(results, identifier, statistic = NULL) {
    # Get the parameters
    res <- results %>%
        select(parameter, estimate, p.value, effect_size) %>%
        mutate_at(vars(parameter, effect_size), ~ formatC(., digits = 2, format = "f")) %>%
        mutate_at(vars(p.value),                ~ formatC(., digits = 3, format = "f"))

    # Report the test
    if (is.null(statistic)) {
        with(res,
            paste0("_t_(", res$parameter, ") = ", estimate, ", _p_ = ", p.value, ", _d_ = ", 
                   effect_size)
             )
    } else {
    # Report a specific statistic
        res[statistic]
    }
}
