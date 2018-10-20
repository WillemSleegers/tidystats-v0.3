context("test-t_tests")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("one sample t-tests work", {
  correct <- test_results[["t_test_one_sample"]]

  model <- t.test(cox$call_parent, alternative = "greater")
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("two sample t-tests work", {
  correct <- test_results[["t_test_two_sample"]]

  model <- t.test(call_parent ~ condition, data = cox, var.equal = TRUE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Welch two sample t-tests work", {
  correct <- test_results[["t_test_welch"]]

  model <- t.test(call_parent ~ condition, data = cox, var.equal = FALSE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Paired t-tests work", {
  correct <- test_results[["t_test_paired"]]

  model <- t.test(cox$affect_positive, cox$affect_negative, paired = TRUE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})