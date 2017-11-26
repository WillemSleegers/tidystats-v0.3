
# tidytext 0.1.1

* Removed spaces from terms in aov() output
* Improved add_stats_to_model(). The method previously required a term and did not automatically complete information (e.g., method information).
* Improved report() function. The method now supports the option to retrieve a single statistic from any tidy stats data frame. This will allow you to report all statistics, even when reporting functions for a specific method is not yet supported.
* Added identifier check to report(). The function will now throw an error when the identifier does not exist.
* Added statistic check to all report functions. The function will now throw an error when the statistic does not exist.

# tidystats 0.1.0

* Initial release
