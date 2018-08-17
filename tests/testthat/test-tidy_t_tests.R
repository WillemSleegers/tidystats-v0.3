context("test-tidy_t_tests")

test_that("one sample t-tests work", {

  # Run t-test
  model <- t.test(cox$call_parent, alternative = "greater")

  # Manually create correct output
  correct <- data.frame(
    statistic = c("mean of x", "t", "df", "p", "95% CI lower", "95% CI upper",
      "null value"),
    value = c(25.775, 24.22477, 199, 1.458065e-61, 24.0167, Inf, 0),
    method = "One Sample t-test",
    notes = "alternative hypothesis: greater",
    stringsAsFactors = FALSE
  )

  # Get tidy stats output
  output <- tidy_stats(model)

  # Compare output with the correct answer
  expect_equal(data.frame(output), correct, tolerance = 0.0000001)
})
