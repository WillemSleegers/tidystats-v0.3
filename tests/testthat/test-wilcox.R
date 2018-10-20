context("test-wilcox")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("Wilcoxon signed rank tests work", {
  correct <- test_results[["wilcoxon_signed_rank"]]

  x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

  model <- wilcox.test(x, y, paired = TRUE, alternative = "greater")
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Wilcoxon rank sum tests with continuity correction work", {
  correct <- test_results[["wilcoxon_rank_sum_continuity"]]

  x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

  model <- suppressWarnings(wilcox.test(Ozone ~ Month, data = airquality,
    subset = Month %in% c(5, 8)))
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Wilcoxon rank sum tests work", {
  correct <- test_results[["wilcoxon_rank_sum"]]

  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

  model <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
    correct = FALSE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Wilcoxon rank sum tests with confidence intervals work", {
  correct <- test_results[["wilcoxon_rank_sum_conf"]]

  x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
  y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

  model <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})