
# tidytext 0.1.1

## New features

* Removed spaces from terms in `aov()` output
* Improved `add_stats_to_model()`. The method previously required a term and did not automatically complete information (e.g., method information).
* Improved `report()` function. The method now supports the option to retrieve a single statistic from any tidy stats data frame. This will allow you to report all statistics, even when reporting functions for a specific method are not yet supported.
* Added identifier check to `report()`. The function will now throw an error when the identifier does not exist.
* Added statistic check to all report functions. The function will now throw an error when the statistic does not exist.
* `write_stats()` now prettifies the numbers using `prettyNum()` when saving them to disk.
* Improved `report_p_value()` to support multiple p-values.
* Added skewness and kurtosis to `describe()`
* Added new `total()` function to calculate count descriptives of categorical data. Also added a `tidy_total()` function to tidy the output of this new function.

## Changes

* Changed `describe()` to no longer accept non-numeric variables, but added the feature that descriptives can be calculated for more than 1 variable at a time. It is recommended to use the `total()` function for non-numeric variables.
* Changed `tidy_descriptives()` to incorporate changes made to `describe()`. A notable change is that term information is now returned to identify which descriptives belong to which variable. Also changed the group delimiter to ' - '.

## Bugfixes

* Fixed bug in report functions that would occur when no statistic argument was provided.

# tidystats 0.1.0

* Initial release
