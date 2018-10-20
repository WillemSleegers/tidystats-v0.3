context("test-chi-squared")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("pearson chi-squared tests work", {
  correct <- test_results[["chi_squared"]]

  M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
  dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent",
    "Republican"))

  model <- chisq.test(M)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("pearson chi-squared tests with Yates correction work", {
  correct <- test_results[["chi_squared_yates"]]

  model <- chisq.test(cox$condition, cox$sex)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("chi-squared tests for given probabilities work", {
  correct <- test_results[["chi_squared_prob"]]

  x <- c(A = 20, B = 15, C = 25)
  model <- chisq.test(x)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
