context("test-correlations")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("pearson correlations work", {
  correct <- test_results[["correlation_pearson"]]

  model <- cor.test(cox$call_parent, cox$anxiety, method = "pearson")
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("kendall correlations work", {
  correct <- test_results[["correlation_kendall"]]

  model <- cor.test(cox$call_parent, cox$anxiety, method = "kendall")
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("spearman correlations work", {
  correct <- test_results[["correlation_spearman"]]

  model <- suppressWarnings(cor.test(cox$call_parent, cox$anxiety,
    method = "spearman"))
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
