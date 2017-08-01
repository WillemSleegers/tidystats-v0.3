<!-- README.md is generated from README.Rmd. Please edit that file -->

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

## Setup




Start by creating an empty data frame.


```r
results <- new_stats_data_frame()
```

Add a t-test:


```r
ttest <- t.test(1:10, y = c(7:20))
results <- add_stats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y")
```

```
## Error in add_stats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y"): could not find function "add_stats"
```

Add a correlation:


```r
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
corr <- cor.test(x, y)
results <- add_stats(results, corr, "cor_x_y", "Hypothesis", "Correlation between x and y")
```

```
## Error in add_stats(results, corr, "cor_x_y", "Hypothesis", "Correlation between x and y"): could not find function "add_stats"
```

This is the resulting data.frame:


```
## Error in dimnames(x) <- dn: length of 'dimnames' [2] not equal to array extent
```

```
## Error in if (tail(stdout, 1) == "") {: argument is of length zero
```
