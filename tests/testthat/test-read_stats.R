tmp <- tempfile()

setup({

  lines <- c("identifier,group,term_nr,term,statistic,value,method,notes",
             paste0("t_test_one_sample,,,,p,0.05,",
                    "One Sample t-test,alternative hypothesis: greater"),
             paste0("correlation_pearson,,,,r,0.2,",
                    "Pearson's product-moment correlation,",
                    "alternative hypothesis: two.sided")
             )
  writeLines(lines, tmp)

})
teardown({
  unlink(tmp)
})

test_that("The .csv is converted to a tidystats named list of tibbles", {

  expected <- list(
    correlation_pearson = tibble::tribble(
      ~statistic, ~value, ~method, ~notes,
      "r", 0.2,"Pearson's product-moment correlation",
      "alternative hypothesis: two.sided"), 
    t_test_one_sample = tibble::tribble(
      ~statistic, ~value, ~method, ~notes,
      "p", 0.05, "One Sample t-test",
      "alternative hypothesis: greater")
    )
  
  actual <- expr(read_stats(tmp))
  expect_identical(names(!!actual), names(expected))
  expect_identical((!!actual)[["correlation_pearson"]], expected[["correlation_pearson"]])
  expect_identical((!!actual)[["t_test_one_sample"]], expected[["t_test_one_sample"]])
  
})

test_that("read_stats is silent", {
  expect_silent(read_stats(tmp))
})
