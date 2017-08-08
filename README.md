<!-- README.md is generated from README.Rmd. Please edit that file -->

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

tidytext is a package to easily create a text file containing the output of statistical models. The goal of this package is to have researchers accompany their manuscript with an organized data file of statistical results in order to greatly improve the reliability of meta-research, and especially to reduce the efforts needed to perform this type of research.

Besides enabling you to create an organized data file of statistical results, the tidytext package also contains functions to help you report statistics in APA style using [R Markdown](http://rmarkdown.rstudio.com). Additionally, the package also contains improved statistical tests that produce output that is often preferred but not necessarily delivered by standard methods (e.g., effect size estimates in *t*-tests).

Please see below for instructions on how to install and use this package. Do note that the package is currently in development and may contain bugs.

### Installation

Tidystats can be installed from Github using [devtools](https://github.com/hadley/devtools). 


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

The main function is `add_stats()`. The function has 2 necessary arguments:

- `results`: The list you want to add the statistical model to
- `model`: The statistical model you want to add to the list

Optionally you can also add an identifier, type, and description of the model using the `identifier`, `type`, and `description` argument, respectively. 

The identifier is used to identify the model (e.g., 'M1'). If you do not provide one, one is automatically created for you. 

The type argument is used to indicate whether the statistical test is a hypothesis test, manipulation check, contrast analysis, or other kind of analysis. This can be used to distinguish the vital statistical tests from those less relevant.

The description argument is used to add additional information which you may find fruitful.

### Add statistical tests

The example below shows how to perform a *t*-test and add it to the results list.


```r
model_t_test <- t.test(1:10, y = c(7:20))
results <- add_stats(results, model_t_test, identifier = "M1", "hypothesis")
```

This example shows how to add a correlation test.


```r
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

model_correlation <- cor.test(x, y)
results <- add_stats(results, model_correlation, "M2", "hypothesis")
```

### Result

Having added the statistical results to the list, you can convert the list to a table or to a data file, ready for sharing. The example below shows how to produce a table containing all of the statistical results.


|identifier |method                               |   estimate| statistic| df_error|   p_value|type       |
|:----------|:------------------------------------|----------:|---------:|--------:|---------:|:----------|
|M1         |Welch Two Sample t-test              | -8.0000000| -5.434930| 21.98221| 0.0000186|hypothesis |
|M2         |Pearson's product-moment correlation |  0.5711816|  1.841083|  7.00000| 0.1081731|hypothesis |

In the 'example' folder you can also find examples of data files containing the output of multiple statistical tests, as well as a markdown file demonstrating how tidystats can be used to create APA-styled statistical reports.
