# tidystats 0.3

## Changes

* Changed the argument order in the family of `add_stats()` functions. Previously, the model output or tidy data frame was the first argument. This allowed you to directly pipe the model output into `add_stats()` (using **magrittr**'s %>%). However, an alternative approach is to have the tidystats list to be the first argument. This allows you create a long sequence of pipes. You start with the results list, add a model via `add_stats()`, pipe the result into the next `add_stats()`, and so on. Since you often store your model output in variable names anyway, this is probably more convenient. Additionally, this probably also keeps your script more tidy (you can do this at the end of your data analysis script).

## Features

* Added support for lme4's `lmer()` and lmerTest's `lmer()`.
* Added support for psych's `alpha()`.
* Added support for metafor's `rma()`.
* Added support for stats' `confint()` via the new `class` argument in `add_stats()` and `add_stats_to_model()`.

## Improvements

* Added check for an existing identifier in `add_stats_to_model()`.
* Added a `class` argument to `add_stats()` and `add_stats_to_model()`. Some statistical tests return a normal data.frame or matrix, which does not specify which test produced the results. This makes it difficult for tidystats to figure out how to tidy the result. Previously, we solved this by `add_stats()` accepting pre-tidied data frames. Now we added a the `class` argument to specify the name of the function that produced the results, so that we can then tidy it for you.
* Added warnings in case unsupported output is added (e.g., a pre-tided data frame).

# tidystats 0.2

## Changes

* Renamed `describe()` to `describe_data()` so that it no longer conflicts with **psych**'s `describe()`.
* Changed `describe_data()` to no longer accept non-numeric variables, but added the feature that descriptives can be calculated for more than 1 variable at a time. It is recommended to use the `count_data()` function for non-numeric variables.
* Renamed `tidy_descriptives()` to `tidy_describe_data()` and improved the function. A notable change is that var information is now returned to identify which descriptives belong to which variable. Also changed the group delimiter to ' - '.
* `write_stats()` now prettifies the numbers using `prettyNum()` when saving them to disk.

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

## Bugfixes

* Fixed bug that it was assumed that confidence intervals in `htests` were always 95% confidence intervals.
* Fixed bug in report functions that would occur when no statistic argument was provided.
* Removed spaces from terms in `aov()` output.
* Removed a leading space from the method information of a Two Sample t-test.
* Improved `add_stats_to_model()`. The method previously required a term and did not automatically complete information (e.g., method information).

# tidystats 0.1

* Initial release
