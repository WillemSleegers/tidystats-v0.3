<!-- README.md is generated from README.Rmd. Please edit that file -->

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

`tidytext` is a package to easily create a text file containing the output of statistical models. The goal of this package is to have researchers accompany their manuscript with an organized data file of statistical results, based on [tidy data](http://vita.had.co.nz/papers/tidy-data.html) principles, in order to greatly improve the reliability of meta-research, and especially to reduce the efforts needed to perform this type of research.

Besides enabling you to create an organized data file of statistical results, the `tidytext` package also contains functions to help you report statistics in APA style using [R Markdown](http://rmarkdown.rstudio.com).

Please see below for instructions on how to install and use this package. **Do note that the package is currently in development and may contain bugs.**

### Installation

`tidystats` can be installed from Github using [devtools](https://github.com/hadley/devtools). 


```r
library(devtools)
install_github("willemsleegers/tidystats")
```

### Setup

Load the package and start by creating an empty list to store the results of statistical models in.


```r
library(tidystats)

results <- list()
```

### Usage

The main function is `add_stats()`. The function has 2 necessary arguments:

- `results`: The list you want to add the statistical model to
- `model`: The statistical model you want to add to the list

Optionally you can also add an identifier, type, and description of the model using the `identifier`, `type`, and `description` arguments, respectively. 

The identifier is used to identify the model (e.g., 'M1'). If you do not provide one, one is automatically created for you. 

The type argument is used to indicate whether the statistical test is a hypothesis test, manipulation check, contrast analysis, or other kind of analysis. This can be used to distinguish the vital statistical tests from those less relevant.

The description argument is used to add additional information which you may find fruitful.

The example below shows how to perform a *t*-test and add it to the results list.


```r
model_t_test <- t.test(1:10, y = c(7:20))
results <- add_stats(results, model_t_test, identifier = "M1", "hypothesis")
```

```
## Error in tidy_stats(model): object 'model' not found
```

This example shows how to add a correlation test.


```r
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

model_correlation <- cor.test(x, y)
results <- add_stats(results, model_correlation, "M2", "other")
```

```
## Error in tidy_stats(model): object 'model' not found
```

### Output

Having added the statistical results to the list, you can convert the list to a table or to a data file, ready for sharing. The example below shows how to produce a table containing all of the statistical results.


```r
library(dplyr)
library(knitr)

results %>%
  stats_list_to_df() %>%
  kable()
```

```
## Error in overscope_eval_next(overscope, expr): object 'identifier' not found
```

To write the results to a file, use `write_stats()`. This produces a .csv file that can be shared online and that can also be used to write your Results section. In the 'example' folder you can find an examples of a data file containing the output of multiple statistical tests, as well as a markdown file demonstrating how `tidystats` can be used to create APA-styled statistical reports.

### Helper functions

#### Descriptives

Since it's common to also report descriptives in addition to the statistical results, we have added a hopefully useful `describe()` function to calculate common descriptive statistics that can be tidied and added to a results data frame. Several examples follow using the `starwars` data.


```r
# Descriptives of the 'height' variable
describe(starwars, height)
```

```
## # A tibble: 1 x 10
##   missing     n     M    SD    SE   min   max range median  mode
##     <int> <int> <dbl> <dbl> <dbl> <int> <int> <int>  <int> <int>
## 1       6    81   174    35   3.9    66   264   198    180   183
```


```r
# By gender
starwars %>%
  group_by(gender) %>%
  describe(height)
```

```
## # A tibble: 5 x 11
## # Groups:   gender [5]
##          gender missing     n     M    SD    SE   min   max range median
##           <chr>   <int> <int> <dbl> <dbl> <dbl> <int> <int> <int>  <int>
## 1        female       2    17   165    23   5.6    96   213   117    166
## 2 hermaphrodite       0     1   175    NA    NA   175   175     0    175
## 3          male       3    59   179    35   4.6    66   264   198    183
## 4          none       1     1   200    NA    NA   200   200     0    200
## 5          <NA>       0     3   120    41  23.5    96   167    71     97
## # ... with 1 more variables: mode <int>
```


```r
# Descriptives of a non-numeric variable
describe(starwars, gender)
```

```
## # A tibble: 5 x 3
## # Groups:   gender [5]
##          gender     n   pct
##           <chr> <int> <dbl>
## 1        female    19  21.8
## 2 hermaphrodite     1   1.1
## 3          male    62  71.3
## 4          none     2   2.3
## 5          <NA>     3   3.4
```

If you use the `describe()` function from the `tidystats` package to get the descriptives, you can use the `tidy_descriptives()` function to tidy the output, and consequently add it to a results list. 


```r
# Adding descriptives to a results list
results <- list()

starwars %>%
  describe(height) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "height", type = "d")
```

```
## $height
## # A tibble: 10 x 3
##    statistic value         type
##        <chr> <dbl>        <chr>
##  1   missing   6.0 descriptives
##  2         n  81.0 descriptives
##  3         M 174.4 descriptives
##  4        SD  34.8 descriptives
##  5        SE   3.9 descriptives
##  6       min  66.0 descriptives
##  7       max 264.0 descriptives
##  8     range 198.0 descriptives
##  9    median 180.0 descriptives
## 10      mode 183.0 descriptives
```
In the `add_stats()` function you can also specify which of the statistics you would like to store in the results list.


```r
# Adding some of the descriptives to a results list
results <- list()

starwars %>%
  describe(height) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "height", type = "d", statistics = c("n", "M", "SD"))
```

```
## $height
## # A tibble: 3 x 3
##   statistic value         type
##       <chr> <dbl>        <chr>
## 1         n    81 descriptives
## 2         M   174 descriptives
## 3        SD    35 descriptives
```
