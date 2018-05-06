
# tidystats 0.2

## New features

* Improved `report()` function. The method now supports the option to retrieve a single statistic from any tidy stats data frame. This will allow you to report all statistics, even when reporting functions for a specific method are not yet supported.
* Added quick report functions for means and standard deviations. Instead of using `report()` you can use `M()` and `SD()` to quickly report the mean or standard deviation, without having to specify that particular statistic. Less typing!
* Added an option called 'tidystats_list' in `options()` to set a default list. By setting the tidystats list in `options()`, you do not need to specify the list in the **results** argument of `report()`. Less typing!
* Reporting regression results will now include a check for whether confidence intervals are included, and report them.
* Added skewness and kurtosis to `describe_data()`
* Added new `count_data()` function to calculate count descriptives of categorical data. Also added a `tidy_count_data()` function to tidy the output of this new function.
* Added support for `chisq.test` and `wilcox.test`.
* Added a better default `identifier` to `add_stats()`. If you supply a variable to be added to the tidystats list, and no identifier is provided, it will take the variable name as the identifier. If you pipe the results into `add_stats()` then the old default identifier will be used (e.g., "M1"). 

## Improvements

* Added identifier check to `report()`. The function will now throw an error when the identifier does not exist.
* Added statistic check to all report functions. The function will now throw an error when the statistic does not exist.
* Improved `report_p_value()` to support multiple p-values.
* Updated documentation to be more consistent and to take into account the changes made in the current update.

## Changes

* Renamed `describe()` to `describe_data()` so that it no longer conflicts with **psych**'s `describe()`.
* Changed `describe_data()` to no longer accept non-numeric variables, but added the feature that descriptives can be calculated for more than 1 variable at a time. It is recommended to use the `count_data()` function for non-numeric variables.
* Renamed `tidy_descriptives()` to `tidy_describe_data()` and improved the function. A notable change is that var information is now returned to identify which descriptives belong to which variable. Also changed the group delimiter to ' - '.
* `write_stats()` now prettifies the numbers using `prettyNum()` when saving them to disk.

## Bugfixes

* Fixed bug that it was always assumed that confidence intervals in `htests` were always 95% confidence intervals.
* Fixed bug in report functions that would occur when no statistic argument was provided.
* Removed spaces from terms in `aov()` output.
* Removed a leading space from the method information of a Two Sample t-test.
* Improved `add_stats_to_model()`. The method previously required a term and did not automatically complete information (e.g., method information).

# tidystats 0.1

* Initial release
