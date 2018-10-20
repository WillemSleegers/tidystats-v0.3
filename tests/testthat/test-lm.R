context("test-lm")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("simple linear regressions work", {
  correct <- test_results[["lm_simple"]]

  model <- lm(call_parent ~ condition, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("multiple linear regressions work", {
  correct <- test_results[["lm_multiple"]]

  model <- lm(call_parent ~ condition + anxiety, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("interaction linear regressions work", {
  correct <- test_results[["lm_interaction"]]

  model <- lm(call_parent ~ condition * anxiety, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
