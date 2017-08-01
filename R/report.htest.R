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
report.htest <- function(model, statistic = NULL) {
    # Create a results data.frame
    results <- new_stats_data_frame() %>%
        # Add the t-test info
        add_stats(model, "tmp")

    # Get the parameters
    res <- results %>%
        select(parameter, estimate, p.value, effect_size) %>%
        mutate_at(vars(parameter, effect_size), ~ formatC(., digits = 2, format = "f")) %>%
        mutate(p.value = report_p_value(p.value))

    # Report the test
    if (is.null(statistic)) {
        with(res,
            paste0("*t*(", res$parameter, ") = ", estimate, ", ", p.value, ", *d* = ",
                   effect_size)
        )
    } else {
    # Report a specific statistic
        res[statistic]
    }
}
