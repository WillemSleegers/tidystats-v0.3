<!-- README.md is generated from README.Rmd. Please edit that file -->

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

`tidystats` is a package to easily create a text file containing the output of statistical models. The goal of this package is to have researchers accompany their manuscript with an organized data file of statistical results, based on [tidy data](http://vita.had.co.nz/papers/tidy-data.html) principles, in order to greatly improve the reliability of meta-research and  to reduce the efforts needed to perform this type of research.

Besides enabling you to create an organized data file of statistical results, the `tidystats` package also contains functions to help you report statistics in APA style using [R Markdown](http://rmarkdown.rstudio.com).

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

- `model`: The statistical model you want to add to the list (e.g., the output of `t.test()` or `lm()`)
- `results`: The list you want to add the statistical output to

Optionally you can also add an identifier, type, a subset of the statistics, whether the analysis was confirmatory or exploratory, and additional notes using the `identifier`, `type`, `statistics`, `confirmatory`, and `notes` arguments, respectively. 

The `identifier` is used to identify the model (e.g., 'weight_height_correlation'). If you do not provide one, one is automatically created for you (albeit not a very descriptive one).

The `type` argument is used to indicate whether the statistical test is a hypothesis test, manipulation check, contrast analysis, or other kind of analysis such as descriptives. This can be used to distinguish the vital statistical tests from those less relevant.

The `statistics` argument is used to select a subset of statistics that you want to add to the results list, in case this is desired.

The `confirmatory` argument is used to indicate whether the test was confirmatory or exploratory. It can also be ommitted.

The `notes` argument is used to add additional information which you may find fruitful. Some statistical tests have default `notes` output (e.g., t-tests), which will be overwritten when a `notes` argument is supplied to the `add_stats()` function.

### Supported statistical functions

**Package:** stats

- `t.test()`
- `cor.test()`
- `lm()`
- `aov()`

### Example

In the following example we perform several statistical tests on a data set, add the output of these results to a list, and save the results to a file.

The data set is called `cox` and contains the data of a replication attempt of C.R. Cox, J. Arndt, T. Pyszczynski, J. Greenberg, A. Abdollahi, and S. Solomon (2008, JPSP, 94(4), Exp. 6) by Wissink et al. The replication study was part of the Reproducibility Project (see https://osf.io/ezcuj/). The data set is part of the `tidystats` package.


```r
# Paired t-test
model1 <- t.test(call_parent ~ condition, data = cox, paired = TRUE)
results <- add_stats(model1, results, identifier = "M1")

# Correlation
model2 <- cor.test(cox$call_parent, cox$call_siblings, alternative = "greater")
results <- add_stats(model2, results, identifier = "M2")

# Regression
model3 <- lm(call_parent ~ condition * anxiety , data = cox)
results <- add_stats(model3, results, identifier = "M3")

# ANOVA
model4 <- aov(call_parent ~ condition * sex, data = cox)
results <- add_stats(model4, results, identifier = "M4")
```

Having added the statistical results to the list, you can convert the list to a table or to a data file, ready for sharing. The example below shows how to produce a table containing all of the statistical results.


```r
library(dplyr)
library(knitr)
options(knitr.kable.NA = '-')

results %>%
  stats_list_to_df() %>%
  kable()
```



|identifier | term_nr|term                                |statistic               |         value|method                               |notes          |
|:----------|-------:|:-----------------------------------|:-----------------------|-------------:|:------------------------------------|:--------------|
|M1         |       -|-                                   |mean of the differences |    -2.7700000|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |t                       |    -1.2614135|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |df                      |    99.0000000|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |p                       |     0.2101241|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |95% CI lower            |    -7.1272396|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |95% CI upper            |     1.5872396|Paired t-test                        |two.sided test |
|M1         |       -|-                                   |null value              |     0.0000000|Paired t-test                        |two.sided test |
|M2         |       -|-                                   |cor                     |    -0.0268794|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |t                       |    -0.3783637|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |df                      |   198.0000000|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |p                       |     0.6472171|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |95% CI lower            |    -0.1430882|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |95% CI upper            |     1.0000000|Pearson's product-moment correlation |greater test   |
|M2         |       -|-                                   |null value              |     0.0000000|Pearson's product-moment correlation |greater test   |
|M3         |       1|(Intercept)                         |b                       |    29.4466534|Linear regression                    |-              |
|M3         |       1|(Intercept)                         |SE                      |     9.9311192|Linear regression                    |-              |
|M3         |       1|(Intercept)                         |t                       |     2.9650891|Linear regression                    |-              |
|M3         |       1|(Intercept)                         |p                       |     0.0034017|Linear regression                    |-              |
|M3         |       1|(Intercept)                         |df                      |   196.0000000|Linear regression                    |-              |
|M3         |       2|conditionmortality salience         |b                       |    20.2945974|Linear regression                    |-              |
|M3         |       2|conditionmortality salience         |SE                      |    14.0193962|Linear regression                    |-              |
|M3         |       2|conditionmortality salience         |t                       |     1.4476085|Linear regression                    |-              |
|M3         |       2|conditionmortality salience         |p                       |     0.1493242|Linear regression                    |-              |
|M3         |       2|conditionmortality salience         |df                      |   196.0000000|Linear regression                    |-              |
|M3         |       3|anxiety                             |b                       |    -1.5511207|Linear regression                    |-              |
|M3         |       3|anxiety                             |SE                      |     3.0119376|Linear regression                    |-              |
|M3         |       3|anxiety                             |t                       |    -0.5149910|Linear regression                    |-              |
|M3         |       3|anxiety                             |p                       |     0.6071396|Linear regression                    |-              |
|M3         |       3|anxiety                             |df                      |   196.0000000|Linear regression                    |-              |
|M3         |       4|conditionmortality salience:anxiety |b                       |    -5.5666889|Linear regression                    |-              |
|M3         |       4|conditionmortality salience:anxiety |SE                      |     4.3104789|Linear regression                    |-              |
|M3         |       4|conditionmortality salience:anxiety |t                       |    -1.2914316|Linear regression                    |-              |
|M3         |       4|conditionmortality salience:anxiety |p                       |     0.1980750|Linear regression                    |-              |
|M3         |       4|conditionmortality salience:anxiety |df                      |   196.0000000|Linear regression                    |-              |
|M3         |       5|(Model)                             |R squared               |     0.0360246|Linear regression                    |-              |
|M3         |       5|(Model)                             |adjusted R squared      |     0.0212698|Linear regression                    |-              |
|M3         |       5|(Model)                             |F                       |     2.4415618|Linear regression                    |-              |
|M3         |       5|(Model)                             |numerator df            |     3.0000000|Linear regression                    |-              |
|M3         |       5|(Model)                             |denominator df          |   196.0000000|Linear regression                    |-              |
|M3         |       5|(Model)                             |p                       |     0.0655150|Linear regression                    |-              |
|M4         |       1|condition                           |df                      |     1.0000000|ANOVA                                |-              |
|M4         |       1|condition                           |SS                      |   383.6450000|ANOVA                                |-              |
|M4         |       1|condition                           |MS                      |   383.6450000|ANOVA                                |-              |
|M4         |       1|condition                           |F                       |     1.7299360|ANOVA                                |-              |
|M4         |       1|condition                           |p                       |     0.1899557|ANOVA                                |-              |
|M4         |       2|sex                                 |df                      |     1.0000000|ANOVA                                |-              |
|M4         |       2|sex                                 |SS                      |  1140.4861329|ANOVA                                |-              |
|M4         |       2|sex                                 |MS                      |  1140.4861329|ANOVA                                |-              |
|M4         |       2|sex                                 |F                       |     5.1426918|ANOVA                                |-              |
|M4         |       2|sex                                 |p                       |     0.0244352|ANOVA                                |-              |
|M4         |       3|condition:sex                       |df                      |     1.0000000|ANOVA                                |-              |
|M4         |       3|condition:sex                       |SS                      |    66.1529617|ANOVA                                |-              |
|M4         |       3|condition:sex                       |MS                      |    66.1529617|ANOVA                                |-              |
|M4         |       3|condition:sex                       |F                       |     0.2982976|ANOVA                                |-              |
|M4         |       3|condition:sex                       |p                       |     0.5855728|ANOVA                                |-              |
|M4         |       4|Residuals                           |df                      |   196.0000000|ANOVA                                |-              |
|M4         |       4|Residuals                           |SS                      | 43466.5909054|ANOVA                                |-              |
|M4         |       4|Residuals                           |MS                      |   221.7683209|ANOVA                                |-              |

To write the results to a file, use `write_stats()` with the results list as the first argument.


```r
write_stats(results, "data/results.csv")
```

This produces a .csv file that can be shared and that can also be used to write your Results section. The report functions will be demonstrated below.

### Report functions

To start reporting your results, first load in the previously saved data file containing the results. This will create a list, just like it was when it was originally saved.


```r
results <- read_stats("data/results.csv")
```

The main function for reporting is `report()`. To figure out how to report the output in APA style, `tidystats` uses the **method** information stored in the results list. For example, the model with identifier 'M1' is a paired t-test. `tidystats` will parse this, see that it is part of the t-test family, and produce results accordingly. `tidystats()` also has test-specific reporting functions, such as `report_t_test()` that are used under the hood, but they are also available for you to use.

Below we show a list of common report examples:

| code                                                         | output                                                          |
|--------------------------------------------------------------|-----------------------------------------------------------------|
|`report(results, "M1")`                                       | *t*(99) = -1.26, *p* = .21                                       |
|`report(results, "M1", statistic = "t")`                      | -1.26                      |
|`report(results, "M2")`                                       | *r*(198) = -.027, *p* = .65                                       |
|`report(results, "M3", term = "conditionmortality salience")` | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15 |
|`report(results, "M3", term_nr = 2`                           | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15                          |
|`report(results, "M3", term = "(Model)")`                     | adjusted *R*<sup>2</sup> = .021, *F*(3, 196) = 2.44, *p* = .066                     |
|`report(results, "M4", term = "condition:sex")`               | *F*(1, 196) = 0.30, *p* = .59               |

As you can see in the examples above, you can use `report()` to produce a full line of output when a model identifier is provided (and a term when the model consists of multiple terms). You can also only retrieve a single statistic by using the `statistic` argument. Additionally, you can refer to terms using either the term label or the term number. Although this latter method might be less descriptive, it reduces the amount of code clutter in your Markdown document. Our philosophy is, in line with Markdown's general writing philosophy, that the code should not distract from writing. To illustrate, writing part of a results section will now, using `tidystats` look like this:

> We found no significant difference between the mortality salience condition and the dental pain condition on the number of minutes allocated to calling one's parents, <code>r report(results, "M1")</code>.

To execute the code, the code segment should be surrounded by backward ticks (see http://rmarkdown.rstudio.com/lesson-4.html), which results in:

> We found no significant difference between the mortality salience condition and the dental pain condition on the number of minutes allocated to calling one's parents, *t*(99) = -1.26, *p* = .21.

### Helper functions

#### Descriptives

Since it's common to also report descriptives in addition to the statistical results, we have added a hopefully useful `describe()` function to calculate common descriptive statistics that can be tidied and added to a results data frame. Several examples follow using the `cox` data.


```r
# Descriptives of the 'anxiety' variable
describe(cox, anxiety)
```

```
## # A tibble: 1 x 10
##   missing     n       M        SD         SE   min   max range median
##     <int> <int>   <dbl>     <dbl>      <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1       0   200 3.21625 0.4917201 0.03476986 1.375 4.375     3   3.25
## # ... with 1 more variables: mode <dbl>
```


```r
# By condition
cox %>%
  group_by(condition) %>%
  describe(anxiety)
```

```
## # A tibble: 2 x 11
## # Groups:   condition [2]
##            condition missing     n      M        SD         SE   min   max
##                <chr>   <int> <int>  <dbl>     <dbl>      <dbl> <dbl> <dbl>
## 1        dental pain       0   100 3.2600 0.4967317 0.04967317 1.625 4.375
## 2 mortality salience       0   100 3.1725 0.4851910 0.04851910 1.375 4.375
## # ... with 3 more variables: range <dbl>, median <dbl>, mode <dbl>
```


```r
# Descriptives of a non-numeric variable
describe(cox, condition)
```

```
## # A tibble: 2 x 3
## # Groups:   condition [2]
##            condition     n   pct
##                <chr> <int> <dbl>
## 1        dental pain   100    50
## 2 mortality salience   100    50
```

If you use the `describe()` function from the `tidystats` package to get the descriptives, you can use the `tidy_descriptives()` function to tidy the output, and consequently add it to a results list. 


```r
# Adding descriptives to a results list
results <- list()

cox %>%
  describe(anxiety) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "anxiety", type = "d", notes = "Anxious attachment style")
```

```
## $anxiety
## # A tibble: 10 x 4
##    statistic        value         type                    notes
##        <chr>        <dbl>        <chr>                    <chr>
##  1   missing   0.00000000 descriptives Anxious attachment style
##  2         n 200.00000000 descriptives Anxious attachment style
##  3         M   3.21625000 descriptives Anxious attachment style
##  4        SD   0.49172007 descriptives Anxious attachment style
##  5        SE   0.03476986 descriptives Anxious attachment style
##  6       min   1.37500000 descriptives Anxious attachment style
##  7       max   4.37500000 descriptives Anxious attachment style
##  8     range   3.00000000 descriptives Anxious attachment style
##  9    median   3.25000000 descriptives Anxious attachment style
## 10      mode   3.50000000 descriptives Anxious attachment style
```
In the `add_stats()` function you can also specify which of the statistics you would like to store in the results list, using the `statistics` argument. Of course, the results can also be tidied when the data is grouped.
