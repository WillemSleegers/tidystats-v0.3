<!-- README.md is generated from README.Rmd. Please edit that file -->

<p align="center">
  <img src="https://raw.githubusercontent.com/WillemSleegers/tidystats/master/docs/img/hex.png" width = 150 align = center alt="tidystats logo"/>
</p>

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

`tidystats` is a package to easily create a text file containing the output of 
statistical models. The goal of this package is to help researchers accompany 
their manuscript with an organized data file of statistical results in order to 
greatly improve the reliability of meta-research and to reduce statistical 
reporting errors.

To make this possible, `tidystats` relies on 
[tidy data](http://vita.had.co.nz/papers/tidy-data.html) principles to combine 
the output of statistical analyses such as *t*-tests, correlations, ANOVAs, and 
regression.

Besides enabling you to create an organized data file of statistical results, 
the `tidystats` package also contains functions to help you report statistics in
APA style. Results can be reported using 
[R Markdown](http://rmarkdown.rstudio.com) or using a new built-in Shiny app. Additionally, development has started on a Google Docs plugin that uses a 
tidystats data file to report statistics.

Please see below for instructions on how to install and use this package. 
**Do note that the package is currently in development. This means the package 
may contain bugs and is subject to significant changes.** If you find any bugs 
or if you have any feedback, please let me know by creating an issue here on 
Github (it's really easy to do!).

### Installation

`tidystats` can be installed from CRAN and the latest version can be installed 
from Github using [devtools](https://github.com/hadley/devtools). 


```r
library(devtools)
install_github("willemsleegers/tidystats")
```

### Setup

Load the package and start by creating an empty list to store the results of 
statistical models in.


```r
library(tidystats)

results <- list()
```

### Usage

The main function is `add_stats()`. The function has 2 necessary arguments:

- `results`: The list you want to add the statistical output to.
- `output`: The output of a statistical test you want to add to the list (e.g., 
the output of `t.test()` or `lm()`)

Optionally you can also add an identifier, type, whether the analysis was 
confirmatory or exploratory, and additional notes using the `identifier`, 
`type`, `confirmatory`, and `notes` arguments, respectively. 

The `identifier` is used to identify the model 
(e.g., 'weight_height_correlation'). If you do not provide one, one is 
automatically created for you.

The `type` argument is used to indicate whether the statistical test is a 
hypothesis test, manipulation check, contrast analysis, or other kind of 
analysis such as descriptives. This can be used to distinguish the vital 
statistical tests from those less relevant.

The `confirmatory` argument is used to indicate whether the test was 
confirmatory or exploratory. It can also be ommitted.

The `notes` argument is used to add additional information which you may find 
fruitful. Some statistical tests have default `notes` output (e.g., t-tests), 
which will be overwritten when a `notes` argument is supplied to the 
`add_stats()` function.

### Supported statistical functions

**Package:** stats

- `t.test()`
- `cor.test()`
- `lm()`
- `glm()`
- `aov()`
- `chisq.test()`
- `wilcox.test()`
- `fisher.test()`

**Package:** psych

- `alpha()`
- `corr.test()`
- `ICC()`

**Package:** lme4 and lmerTest

- `lmer()`

### Example

In the following example we perform several statistical tests on a data set, 
add the output of these results to a list, and save the results to a file.

The data set is called `cox` and contains the data of a replication attempt of 
C.R. Cox, J. Arndt, T. Pyszczynski, J. Greenberg, A. Abdollahi, and S. Solomon 
(2008, JPSP, 94(4), Exp. 6) by Wissink et al. The replication study was part of 
the Reproducibility Project (see https://osf.io/ezcuj/). The data set is part of
the `tidystats` package.


```r
# Perform analyses
M1_condition <- t.test(call_parent ~ condition, data = cox, paired = TRUE)
M2_parent_siblings <- cor.test(cox$call_parent, cox$call_siblings, 
  alternative = "greater")
M3_condition_anxiety <- lm(call_parent ~ condition * anxiety , data = cox)
M4_condition_sex <- aov(call_parent ~ condition * sex, data = cox)

# Add results
results <- results %>%
  add_stats(M1_condition) %>%
  add_stats(M2_parent_siblings) %>%
  add_stats(M3_condition_anxiety) %>%
  add_stats(M4_condition_sex)
```

To write the results to a file, use `write_stats()` with the results list as the
first argument.


```r
write_stats(results, "data/results.csv")
```

To see how the data was actually tidied, you can open the .csv file or you can 
convert the tidystats results list to a table, as shown below.


```r
library(dplyr)
library(knitr)
options(knitr.kable.NA = '-')

results %>%
  stats_list_to_df() %>%
  select(-notes) %>%
  kable()
```



|identifier           |group        | term_nr|term                                |statistic               |         value|method                               |
|:--------------------|:------------|-------:|:-----------------------------------|:-----------------------|-------------:|:------------------------------------|
|M1_condition         |-            |       -|-                                   |mean of the differences |    -2.7700000|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |t                       |    -1.2614135|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |df                      |    99.0000000|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |p                       |     0.2101241|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |95% CI lower            |    -7.1272396|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |95% CI upper            |     1.5872396|Paired t-test                        |
|M1_condition         |-            |       -|-                                   |null value              |     0.0000000|Paired t-test                        |
|M2_parent_siblings   |-            |       -|-                                   |r                       |    -0.0268794|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |t                       |    -0.3783637|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |df                      |   198.0000000|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |p                       |     0.6472171|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |95% CI lower            |    -0.1430882|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |95% CI upper            |     1.0000000|Pearson's product-moment correlation |
|M2_parent_siblings   |-            |       -|-                                   |null value              |     0.0000000|Pearson's product-moment correlation |
|M3_condition_anxiety |coefficients |       1|(Intercept)                         |b                       |    29.4466534|Linear model                         |
|M3_condition_anxiety |coefficients |       1|(Intercept)                         |SE                      |     9.9311192|Linear model                         |
|M3_condition_anxiety |coefficients |       1|(Intercept)                         |t                       |     2.9650891|Linear model                         |
|M3_condition_anxiety |coefficients |       1|(Intercept)                         |p                       |     0.0034017|Linear model                         |
|M3_condition_anxiety |coefficients |       1|(Intercept)                         |df                      |   196.0000000|Linear model                         |
|M3_condition_anxiety |coefficients |       2|conditionmortality salience         |b                       |    20.2945974|Linear model                         |
|M3_condition_anxiety |coefficients |       2|conditionmortality salience         |SE                      |    14.0193962|Linear model                         |
|M3_condition_anxiety |coefficients |       2|conditionmortality salience         |t                       |     1.4476085|Linear model                         |
|M3_condition_anxiety |coefficients |       2|conditionmortality salience         |p                       |     0.1493242|Linear model                         |
|M3_condition_anxiety |coefficients |       2|conditionmortality salience         |df                      |   196.0000000|Linear model                         |
|M3_condition_anxiety |coefficients |       3|anxiety                             |b                       |    -1.5511207|Linear model                         |
|M3_condition_anxiety |coefficients |       3|anxiety                             |SE                      |     3.0119376|Linear model                         |
|M3_condition_anxiety |coefficients |       3|anxiety                             |t                       |    -0.5149910|Linear model                         |
|M3_condition_anxiety |coefficients |       3|anxiety                             |p                       |     0.6071396|Linear model                         |
|M3_condition_anxiety |coefficients |       3|anxiety                             |df                      |   196.0000000|Linear model                         |
|M3_condition_anxiety |coefficients |       4|conditionmortality salience:anxiety |b                       |    -5.5666889|Linear model                         |
|M3_condition_anxiety |coefficients |       4|conditionmortality salience:anxiety |SE                      |     4.3104789|Linear model                         |
|M3_condition_anxiety |coefficients |       4|conditionmortality salience:anxiety |t                       |    -1.2914316|Linear model                         |
|M3_condition_anxiety |coefficients |       4|conditionmortality salience:anxiety |p                       |     0.1980750|Linear model                         |
|M3_condition_anxiety |coefficients |       4|conditionmortality salience:anxiety |df                      |   196.0000000|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |R squared               |     0.0360246|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |adjusted R squared      |     0.0212698|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |F                       |     2.4415618|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |numerator df            |     3.0000000|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |denominator df          |   196.0000000|Linear model                         |
|M3_condition_anxiety |model        |       -|-                                   |p                       |     0.0655150|Linear model                         |
|M4_condition_sex     |-            |       1|condition                           |df                      |     1.0000000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       1|condition                           |SS                      |   383.6450000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       1|condition                           |MS                      |   383.6450000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       1|condition                           |F                       |     1.7299360|Factorial ANOVA                      |
|M4_condition_sex     |-            |       1|condition                           |p                       |     0.1899557|Factorial ANOVA                      |
|M4_condition_sex     |-            |       2|sex                                 |df                      |     1.0000000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       2|sex                                 |SS                      |  1140.4861329|Factorial ANOVA                      |
|M4_condition_sex     |-            |       2|sex                                 |MS                      |  1140.4861329|Factorial ANOVA                      |
|M4_condition_sex     |-            |       2|sex                                 |F                       |     5.1426918|Factorial ANOVA                      |
|M4_condition_sex     |-            |       2|sex                                 |p                       |     0.0244352|Factorial ANOVA                      |
|M4_condition_sex     |-            |       3|condition:sex                       |df                      |     1.0000000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       3|condition:sex                       |SS                      |    66.1529617|Factorial ANOVA                      |
|M4_condition_sex     |-            |       3|condition:sex                       |MS                      |    66.1529617|Factorial ANOVA                      |
|M4_condition_sex     |-            |       3|condition:sex                       |F                       |     0.2982976|Factorial ANOVA                      |
|M4_condition_sex     |-            |       3|condition:sex                       |p                       |     0.5855728|Factorial ANOVA                      |
|M4_condition_sex     |-            |       4|Residuals                           |df                      |   196.0000000|Factorial ANOVA                      |
|M4_condition_sex     |-            |       4|Residuals                           |SS                      | 43466.5909054|Factorial ANOVA                      |
|M4_condition_sex     |-            |       4|Residuals                           |MS                      |   221.7683209|Factorial ANOVA                      |

### Report functions

There are two ways to report your results using tidystats: Using R Markdown or 
using a built-in Shiny app. In both cases, you need the tidystats list that 
contains the tidied output of your statistical tests.

If you have previously created a tidystats file, you can read in this file to 
re-create the tidystats list, using the `read_stats()` function.


```r
results <- read_stats("data/results.csv")
```

#### Shiny app

If you do not want to use R Markdown, you can use the built-in Shiny app to 
interactively produce APA-output and copy it to your manuscript. To start the
app, run the `inspect()` function. 

The `inspect()` function takes the tidystats list as its first argument, 
optionally followed by one or more identifiers. If no identifiers are provided,
all models will be displayed. The results of each model will be displayed in a 
table and you can click on a row to produce APA output. This APA output will 
appear in a textbox at the bottom, next to a copy button that can be pressed to
copy the results into your clipboard. See below for an example.

<p>
  <img src="https://raw.githubusercontent.com/WillemSleegers/tidystats/master/docs/img/inspect.png" alt="inspect"/>
</p>

#### R Markdown

You can use the `report()` function to report your results via R Markdown. This 
function requires at minimum the tidystats list and an identifier identifying 
the exact test you want to report. It may also be necessary to provide 
additional information, such as a term in a regression, for the `report()` 
function to figure out what you want to report.

To reduce repetition, you can use `options()` to set the default tidystats list 
to use. This way the `report()` function requires one fewer argument. 
You set the default tidystats list by running the following code:


```r
options(tidystats_list = results)
```

To figure out how to report the output in APA style, the `report()` function 
uses the **method** information stored in the tidied model. For example, the 
model with identifier 'M1' is a paired t-test. `report()` will parse this, 
see that it is part of the t-test family, and produce results accordingly. 

Below is a list of common report examples:

| code                                                                  | output                                                                   |
|-----------------------------------------------------------------------|--------------------------------------------------------------------------|
|`report("M1_condition")`                                               | *t*(99) = -1.26, *p* = .21, 95% CI [-7.13, 1.59]                                               |
|`report("M1_condition", statistic = "t")`                              | -1.26                              |
|`report("M2_parent_siblings")`                                         | *r*(198) = -.027, *p* = .65                                         |
|`report("M3_condition_anxiety", term = "conditionmortality salience")` | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15 |
|`report("M3_condition_anxiety", term_nr = 2)`                          | *b* = 20.29, *SE* = 14.02, *t*(196) = 1.45, *p* = .15                          |
|`report("M3_condition_anxiety", term = "(Model)")`                     | adjusted *R*<sup>2</sup> = .0035, *F*(1, 198) = 1.70, *p* = .19          |
|`report("M4_condition_sex", term = "condition:sex")`                   | *F*(1, 196) = 0.30, *p* = .59                   |

As you can see in the examples above, you can use `report()` to produce a full 
line of output. You can also retrieve a single statistic by using the 
`statistic` argument. Additionally, you can refer to terms using either the 
term label or the term number (and in some cases, using a group). Although it
may be less descriptive to use a term number, it reduces the amount of code 
clutter in your Markdown document. Our philosophy is, in line with Markdown's 
general writing philosophy, that the code should not distract from writing. To 
illustrate, writing part of a results section with `tidystats` will look 
like this:

> We found no significant difference between the mortality salience condition 
and the dental pain condition on the number of minutes allocated to calling 
one's parents, <code>r report("M1_condition")</code>.

To execute the code, the code segment should be surrounded by backward ticks 
(see http://rmarkdown.rstudio.com/lesson-4.html), which results in:

> We found no significant difference between the mortality salience condition 
and the dental pain condition on the number of minutes allocated to calling 
one's parents, *t*(99) = -1.26, *p* = .21, 95% CI [-7.13, 1.59].

### Helper functions

#### Descriptives

Since it's common to also report descriptives in addition to the statistical 
results, we have added a hopefully useful `describe_data()` and `count_data()` 
function to calculate common descriptive statistics that can be tidied and added
to a results data frame. Several examples follow using the `cox` data.


```r
# Descriptives of the 'anxiety' variable
describe_data(cox, anxiety)
```

```
## # A tibble: 1 x 13
##   var   missing     n     M    SD     SE   min   max range median  mode
##   <chr>   <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 anxi…       0   200  3.22 0.492 0.0348  1.38  4.38     3   3.25   3.5
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
##   var   condition missing     n     M    SD     SE   min   max range median
##   <chr> <chr>       <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 anxi… dental p…       0   100  3.26 0.497 0.0497  1.62  4.38  2.75   3.38
## 2 anxi… mortalit…       0   100  3.17 0.485 0.0485  1.38  4.38  3      3.25
## # ... with 3 more variables: mode <dbl>, skew <dbl>, kurtosis <dbl>
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

If you use the `describe_data()` and `count_data()` function from the 
`tidystats` package to get the descriptives, you can use the 
`tidy_describe_data()` and `tidy_count_data()` function to tidy the output, and
consequently add it to a results list.

(Note: This will soon be improved)


```r
anxiety_tidy <- cox %>%
  describe_data(anxiety) %>%
  tidy_describe_data()

results <- results %>%
  add_stats(anxiety_tidy, type = "d", notes = "Anxious attachment style")
```

```
## Warning in add_stats.data.frame(., anxiety_tidy, type = "d", notes =
## "Anxious attachment style"): You added a data.frame to your results list.
## Please make sure it is properly tidied.
```
