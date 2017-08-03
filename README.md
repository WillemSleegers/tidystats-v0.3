<!-- README.md is generated from README.Rmd. Please edit that file -->

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/), [Arnoud Plantinga](http://www.arnoudplantinga.nl/)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

## Setup



Start by creating an empty list to store the results of statistical models in.


```r
results <- new_stats_list()
```

## Add a t-test:


```r
model_t_test <- t.test(1:10, y = c(7:20))
results <- add_stats(results, model_t_test, identifier = "M1", "hypothesis")
```

`add_stats()` has 2 necessary arguments:

- `results`: The list you want to add the statistical model to
- `model`: The statistical model you want to add to the list

Optionally you can also add an identifier, type, and description of the model using the `identifier`, `type`, and `description` argument, respectively. 

The identifier is used to uniquely identify the model. If you do not provide one, one is automatically created for you. 

The type argument is used to indicate whether the statistical test is a hypothesis test, manipulation check, contrast analysis, or 'other'. This can be used to distinguish the vital statistical tests from those less relevant (e.g., in the context of meta-analyses).

The description argument is used to add additional information which you may find fruitful.

## Add a correlation:


```r
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

model_correlation <- cor.test(x, y)
results <- add_stats(results, model_correlation, "M2", "hypothesis")
```

## Result


```
## Error in results %>% bind_rows() %>% select(identifier, method, estimate, : could not find function "%>%"
```
