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
results <- addstats(results, ttest, "ttest_x_y", "Hypothesis", "A t-test of x and y")
```

Add a correlation:


```r
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
corr <- cor.test(x, y)
results <- addstats(results, corr, "cor_x_y", "Hypothesis", "Correlation between x and y")
```

Show the results:


```r
library(pander)
panderOptions("table.split.table", 1000)
pander(results, style = "rmarkdown")
```



|  identifier  |    type    |                method                |  estimate  |  statistic  |  p.value  |  parameter  |         description         |
|:------------:|:----------:|:------------------------------------:|:----------:|:-----------:|:---------:|:-----------:|:---------------------------:|
|  ttest_x_y   | Hypothesis |       Welch Two Sample t-test        |     -8     |   -5.435    | 1.855e-05 |    21.98    |     A t-test of x and y     |
|   cor_x_y    | Hypothesis | Pearson's product-moment correlation |   0.5712   |    1.841    |  0.1082   |      7      | Correlation between x and y |


