context("test-psych")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("psych's alpha work", {
  correct <- test_results[["psych_alpha"]]

  model <- psych::alpha(dplyr::select(psych::epi, V1, V3, V8, V10, V13, V17,
    V22, V25, V27, V39, V44, V46, V49, V53, V56))
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("psych's correlations work", {
  correct <- test_results[["psych_correlations"]]

  model <- psych::corr.test(attitude, adjust = "none")
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("psych's ICCs work", {
  correct <- test_results[["psych_ICC"]]

  sf <- matrix(ncol = 4, byrow = TRUE,
    c(9,  2, 5, 8,
      6,  1, 3, 2,
      8,  4, 6, 8,
      7,  1, 2, 6,
      10, 5, 6, 9,
      6,  2, 4, 7))
  colnames(sf) <- paste("J", 1:4, sep = "")
  rownames(sf) <- paste("S", 1:6, sep = "")

  model <- psych::ICC(sf, lmer = FALSE)

  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
