<!-- README.md is generated from README.Rmd. Please edit that file -->

<p align="center">
  <img src="docs/logo/hex.png" width = 150 align = center alt="tidystats logo"/>
</p>

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

`tidystats` is a package to easily create a text file containing the output of statistical models. The goal of this package is to help researchers accompany their manuscript with an organized data file of statistical results in order to greatly improve the reliability of meta-research and to reduce statistical reporting errors.

To make this possible, `tidystats` relies on [tidy data](http://vita.had.co.nz/papers/tidy-data.html) principles to combine the output of statistical analyses such as *t*-tests, correlations, ANOVAs, and regression analyses.

Besides enabling you to create an organized data file of statistical results, the `tidystats` package also contains functions to help you report statistics in APA style using [R Markdown](http://rmarkdown.rstudio.com). Additionally, development has started on a Shiny app and a Google Docs plugin that uses a tidystats data file to report statistics.

Please see below for instructions on how to install and use this package. **Do note that the package is currently in development and may contain bugs.** If you find any, please let me know by creating an issue here on Github (it's really easy to do!).

### Installation

`tidystats` can be installed from CRAN, but the latest version can be installed from Github using [devtools](https://github.com/hadley/devtools). 


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

- `output`: The output of a statistical test you want to add to the list (e.g., the output of `t.test()` or `lm()`)
- `results`: The list you want to add the statistical output to

Optionally you can also add an identifier, type, a subset of the statistics, whether the analysis was confirmatory or exploratory, and additional notes using the `identifier`, `type`, `statistics`, `confirmatory`, and `notes` arguments, respectively. 

The `identifier` is used to identify the model (e.g., 'weight_height_correlation'). If you do not provide one, one is automatically created for you.

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
- `chisq.test()`
- `wilcox.test()`

### Example

In the following example we perform several statistical tests on a data set, add the output of these results to a list, and save the results to a file.

The data set is called `cox` and contains the data of a replication attempt of C.R. Cox, J. Arndt, T. Pyszczynski, J. Greenberg, A. Abdollahi, and S. Solomon (2008, JPSP, 94(4), Exp. 6) by Wissink et al. The replication study was part of the Reproducibility Project (see https://osf.io/ezcuj/). The data set is part of the `tidystats` package.


```r
# Perform analyses
M1_condition <- t.test(call_parent ~ condition, data = cox, paired = TRUE)
M2_parent_siblings <- cor.test(cox$call_parent, cox$call_siblings, alternative = "greater")
M3_condition_anxiety <- lm(call_parent ~ condition * anxiety , data = cox)
M4_condition_sex <- aov(call_parent ~ condition * sex, data = cox)

# Add results
results <- results %>%
  add_stats(M1_condition) %>%
  add_stats(M2_parent_siblings) %>%
  add_stats(M3_condition_anxiety) %>%
  add_stats(M4_condition_sex)
```

Having added the statistical results to the list, you can convert the list to a table or to a data file, ready for sharing. The example below shows how to produce a table containing all of the statistical results.


```r
library(dplyr)
library(knitr)
options(knitr.kable.NA = '-')

results %>%
  stats_list_to_df() %>%
  select(-notes) %>%
  kable()
```



|identifier           | term_nr|term                                |statistic               |         value|method                               |
|:--------------------|-------:|:-----------------------------------|:-----------------------|-------------:|:------------------------------------|
|M1_condition         |       -|-                                   |mean of the differences |    -2.7700000|Paired t-test                        |
|M1_condition         |       -|-                                   |t                       |    -1.2614135|Paired t-test                        |
|M1_condition         |       -|-                                   |df                      |    99.0000000|Paired t-test                        |
|M1_condition         |       -|-                                   |p                       |     0.2101241|Paired t-test                        |
|M1_condition         |       -|-                                   |95% CI lower            |    -7.1272396|Paired t-test                        |
|M1_condition         |       -|-                                   |95% CI upper            |     1.5872396|Paired t-test                        |
|M1_condition         |       -|-                                   |null value              |     0.0000000|Paired t-test                        |
|M2_parent_siblings   |       -|-                                   |cor                     |    -0.0268794|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |t                       |    -0.3783637|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |df                      |   198.0000000|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |p                       |     0.6472171|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |95% CI lower            |    -0.1430882|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |95% CI upper            |     1.0000000|Pearson's product-moment correlation |
|M2_parent_siblings   |       -|-                                   |null value              |     0.0000000|Pearson's product-moment correlation |
|M3_condition_anxiety |       1|(Intercept)                         |b                       |    29.4466534|Linear regression                    |
|M3_condition_anxiety |       1|(Intercept)                         |SE                      |     9.9311192|Linear regression                    |
|M3_condition_anxiety |       1|(Intercept)                         |t                       |     2.9650891|Linear regression                    |
|M3_condition_anxiety |       1|(Intercept)                         |p                       |     0.0034017|Linear regression                    |
|M3_condition_anxiety |       1|(Intercept)                         |df                      |   196.0000000|Linear regression                    |
|M3_condition_anxiety |       2|conditionmortality salience         |b                       |    20.2945974|Linear regression                    |
|M3_condition_anxiety |       2|conditionmortality salience         |SE                      |    14.0193962|Linear regression                    |
|M3_condition_anxiety |       2|conditionmortality salience         |t                       |     1.4476085|Linear regression                    |
|M3_condition_anxiety |       2|conditionmortality salience         |p                       |     0.1493242|Linear regression                    |
|M3_condition_anxiety |       2|conditionmortality salience         |df                      |   196.0000000|Linear regression                    |
|M3_condition_anxiety |       3|anxiety                             |b                       |    -1.5511207|Linear regression                    |
|M3_condition_anxiety |       3|anxiety                             |SE                      |     3.0119376|Linear regression                    |
|M3_condition_anxiety |       3|anxiety                             |t                       |    -0.5149910|Linear regression                    |
|M3_condition_anxiety |       3|anxiety                             |p                       |     0.6071396|Linear regression                    |
|M3_condition_anxiety |       3|anxiety                             |df                      |   196.0000000|Linear regression                    |
|M3_condition_anxiety |       4|conditionmortality salience:anxiety |b                       |    -5.5666889|Linear regression                    |
|M3_condition_anxiety |       4|conditionmortality salience:anxiety |SE                      |     4.3104789|Linear regression                    |
|M3_condition_anxiety |       4|conditionmortality salience:anxiety |t                       |    -1.2914316|Linear regression                    |
|M3_condition_anxiety |       4|conditionmortality salience:anxiety |p                       |     0.1980750|Linear regression                    |
|M3_condition_anxiety |       4|conditionmortality salience:anxiety |df                      |   196.0000000|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |R squared               |     0.0360246|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |adjusted R squared      |     0.0212698|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |F                       |     2.4415618|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |numerator df            |     3.0000000|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |denominator df          |   196.0000000|Linear regression                    |
|M3_condition_anxiety |       5|(Model)                             |p                       |     0.0655150|Linear regression                    |
|M4_condition_sex     |       1|condition                           |df                      |     1.0000000|ANOVA                                |
|M4_condition_sex     |       1|condition                           |SS                      |   383.6450000|ANOVA                                |
|M4_condition_sex     |       1|condition                           |MS                      |   383.6450000|ANOVA                                |
|M4_condition_sex     |       1|condition                           |F                       |     1.7299360|ANOVA                                |
|M4_condition_sex     |       1|condition                           |p                       |     0.1899557|ANOVA                                |
|M4_condition_sex     |       2|sex                                 |df                      |     1.0000000|ANOVA                                |
|M4_condition_sex     |       2|sex                                 |SS                      |  1140.4861329|ANOVA                                |
|M4_condition_sex     |       2|sex                                 |MS                      |  1140.4861329|ANOVA                                |
|M4_condition_sex     |       2|sex                                 |F                       |     5.1426918|ANOVA                                |
|M4_condition_sex     |       2|sex                                 |p                       |     0.0244352|ANOVA                                |
|M4_condition_sex     |       3|condition:sex                       |df                      |     1.0000000|ANOVA                                |
|M4_condition_sex     |       3|condition:sex                       |SS                      |    66.1529617|ANOVA                                |
|M4_condition_sex     |       3|condition:sex                       |MS                      |    66.1529617|ANOVA                                |
|M4_condition_sex     |       3|condition:sex                       |F                       |     0.2982976|ANOVA                                |
|M4_condition_sex     |       3|condition:sex                       |p                       |     0.5855728|ANOVA                                |
|M4_condition_sex     |       4|Residuals                           |df                      |   196.0000000|ANOVA                                |
|M4_condition_sex     |       4|Residuals                           |SS                      | 43466.5909054|ANOVA                                |
|M4_condition_sex     |       4|Residuals                           |MS                      |   221.7683209|ANOVA                                |

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

Additionally, you can use `options()` to set the default tidystats list to use. This way the `report()` functions below require one fewer argument. You set the default tidystats list by running the following code:


```r
options(tidystats_list = results)
```

The main function for reporting is `report()`. To figure out how to report the output in APA style, `tidystats` uses the **method** information stored in the results list. For example, the model with identifier 'M1' is a paired t-test. `tidystats` will parse this, see that it is part of the t-test family, and produce results accordingly. `tidystats()` also has test-specific reporting functions, such as `report_t_test()` that are used under the hood, but they are also available for you to use.

Below we show a list of common report examples:

| code                                                         | output                                                          |
|--------------------------------------------------------------|-----------------------------------------------------------------|
|`report("M1_condition")`                                       | *t*(99) = -1.26, *p* = .21                                       |
|`report("M1_condition", statistic = "t")`                      | -1.26                      |
|`report("M2_parent_siblings")`                                       | *r*(198) = -.027, *p* = .65                                       |
|`report("M3_condition_anxiety", term = "conditionmortality salience")` | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15 |
|`report("M3_condition_anxiety", term_nr = 2)`                           | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15                          |
|`report("M3_condition_anxiety", term = "(Model)")`                     | adjusted *R*<sup>2</sup> = .0035, *F*(1, 198) = 1.70, *p* = .19                     |
|`report("M4_condition_sex", term = "condition:sex")`               | *F*(1, 196) = 0.30, *p* = .59               |

As you can see in the examples above, you can use `report()` to produce a full line of output when a model identifier is provided (and a term when the model consists of multiple terms). You can also only retrieve a single statistic by using the `statistic` argument. Additionally, you can refer to terms using either the term label or the term number. Although this latter method might be less descriptive, it reduces the amount of code clutter in your Markdown document. Our philosophy is, in line with Markdown's general writing philosophy, that the code should not distract from writing. To illustrate, writing part of a results section will now, using `tidystats` look like this:

> We found no significant difference between the mortality salience condition and the dental pain condition on the number of minutes allocated to calling one's parents, <code>r report("M1_condition")</code>.

To execute the code, the code segment should be surrounded by backward ticks (see http://rmarkdown.rstudio.com/lesson-4.html), which results in:

> We found no significant difference between the mortality salience condition and the dental pain condition on the number of minutes allocated to calling one's parents, *t*(99) = -1.26, *p* = .21.

### Helper functions

#### Descriptives

Since it's common to also report descriptives in addition to the statistical results, we have added a hopefully useful `describe_data()` and `count_data()` function to calculate common descriptive statistics that can be tidied and added to a results data frame. Several examples follow using the `cox` data.


```r
# Descriptives of the 'anxiety' variable
describe_data(cox, anxiety)
```

```
## # A tibble: 1 x 13
##   var     missing     n     M    SD     SE   min   max range median  mode
##   <chr>     <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 anxiety       0   200  3.22 0.492 0.0348  1.38  4.38     3   3.25   3.5
## # ... with 2 more variables: skew <dbl>, kurtosis <dbl>
```


```r
# By condition
cox %>%
  group_by(condition) %>%
  describe_data(anxiety)
```

```
## # A tibble: 2 x 14
## # Groups:   condition [2]
##   var     condition     missing     n     M    SD     SE   min   max range
##   <chr>   <chr>           <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
## 1 anxiety dental pain         0   100  3.26 0.497 0.0497  1.62  4.38  2.75
## 2 anxiety mortality sa…       0   100  3.17 0.485 0.0485  1.38  4.38  3   
## # ... with 4 more variables: median <dbl>, mode <dbl>, skew <dbl>,
## #   kurtosis <dbl>
```


```r
# Descriptives of a non-numeric variable
count_data(cox, condition)
```

```
## # A tibble: 2 x 4
##   var       group                  n   pct
##   <chr>     <chr>              <int> <dbl>
## 1 condition dental pain          100    50
## 2 condition mortality salience   100    50
```

If you use the `describe_data()` and `count_data()` function from the `tidystats` package to get the descriptives, you can use the `tidy_describe_data()` and `tidy_count_data()` function to tidy the output, and consequently add it to a results list.

(Note: This will soon be improved)


```r
anxiety_tidy <- cox %>%
  describe_data(anxiety) %>%
  tidy_describe_data()

add_stats(results, anxiety_tidy, type = "d", notes = "Anxious attachment style")
```

```
## $M1_condition
## # A tibble: 7 x 4
##   statistic                 value method        notes                     
##   <chr>                     <dbl> <chr>         <chr>                     
## 1 mean of the differences  -2.77  Paired t-test alternative hypothesis: t…
## 2 t                        -1.26  Paired t-test alternative hypothesis: t…
## 3 df                       99     Paired t-test alternative hypothesis: t…
## 4 p                         0.210 Paired t-test alternative hypothesis: t…
## 5 95% CI lower             -7.13  Paired t-test alternative hypothesis: t…
## 6 95% CI upper              1.59  Paired t-test alternative hypothesis: t…
## 7 null value                0     Paired t-test alternative hypothesis: t…
## 
## $M2_parent_siblings
## # A tibble: 7 x 4
##   statistic        value method                               notes       
##   <chr>            <dbl> <chr>                                <chr>       
## 1 cor            -0.0269 Pearson's product-moment correlation alternative…
## 2 t              -0.378  Pearson's product-moment correlation alternative…
## 3 df            198      Pearson's product-moment correlation alternative…
## 4 p               0.647  Pearson's product-moment correlation alternative…
## 5 95% CI lower   -0.143  Pearson's product-moment correlation alternative…
## 6 95% CI upper    1      Pearson's product-moment correlation alternative…
## 7 null value      0      Pearson's product-moment correlation alternative…
## 
## $M3_condition_anxiety
## # A tibble: 26 x 5
##    term_nr term                        statistic     value method         
##      <dbl> <chr>                       <chr>         <dbl> <chr>          
##  1       1 (Intercept)                 b          29.4     Linear regress…
##  2       1 (Intercept)                 SE          9.93    Linear regress…
##  3       1 (Intercept)                 t           2.97    Linear regress…
##  4       1 (Intercept)                 p           0.00340 Linear regress…
##  5       1 (Intercept)                 df        196       Linear regress…
##  6       2 conditionmortality salience b          20.3     Linear regress…
##  7       2 conditionmortality salience SE         14.0     Linear regress…
##  8       2 conditionmortality salience t           1.45    Linear regress…
##  9       2 conditionmortality salience p           0.149   Linear regress…
## 10       2 conditionmortality salience df        196       Linear regress…
## # ... with 16 more rows
## 
## $M4_condition_sex
## # A tibble: 18 x 5
##    term_nr term          statistic      value method
##      <int> <chr>         <chr>          <dbl> <chr> 
##  1       1 condition     df            1      ANOVA 
##  2       1 condition     SS          384.     ANOVA 
##  3       1 condition     MS          384.     ANOVA 
##  4       1 condition     F             1.73   ANOVA 
##  5       1 condition     p             0.190  ANOVA 
##  6       2 sex           df            1      ANOVA 
##  7       2 sex           SS         1140.     ANOVA 
##  8       2 sex           MS         1140.     ANOVA 
##  9       2 sex           F             5.14   ANOVA 
## 10       2 sex           p             0.0244 ANOVA 
## 11       3 condition:sex df            1      ANOVA 
## 12       3 condition:sex SS           66.2    ANOVA 
## 13       3 condition:sex MS           66.2    ANOVA 
## 14       3 condition:sex F             0.298  ANOVA 
## 15       3 condition:sex p             0.586  ANOVA 
## 16       4 Residuals     df          196      ANOVA 
## 17       4 Residuals     SS        43467.     ANOVA 
## 18       4 Residuals     MS          222.     ANOVA 
## 
## $anxiety_tidy
## # A tibble: 12 x 5
##    var     statistic     value type         notes                   
##    <chr>   <chr>         <dbl> <chr>        <chr>                   
##  1 anxiety missing      0      descriptives Anxious attachment style
##  2 anxiety n          200      descriptives Anxious attachment style
##  3 anxiety M            3.22   descriptives Anxious attachment style
##  4 anxiety SD           0.492  descriptives Anxious attachment style
##  5 anxiety SE           0.0348 descriptives Anxious attachment style
##  6 anxiety min          1.38   descriptives Anxious attachment style
##  7 anxiety max          4.38   descriptives Anxious attachment style
##  8 anxiety range        3      descriptives Anxious attachment style
##  9 anxiety median       3.25   descriptives Anxious attachment style
## 10 anxiety mode         3.5    descriptives Anxious attachment style
## 11 anxiety skew        -0.510  descriptives Anxious attachment style
## 12 anxiety kurtosis     3.73   descriptives Anxious attachment style
```
In the `add_stats()` function you can also specify which of the statistics you would like to store in the results list, using the `statistics` argument. Of course, the results can also be tidied when the data is grouped.
