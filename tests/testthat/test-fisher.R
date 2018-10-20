context("test-fisher")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("Fisher's tests work", {
  correct <- test_results[["fisher_test"]]

  TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2, dimnames = list(Guess =
      c("Milk", "Tea"), Truth = c("Milk", "Tea")))

  model <- fisher.test(TeaTasting)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Fisher's tests without confidence intervals work", {
  correct <- test_results[["fisher_test_no_CI"]]

  Convictions <- matrix(c(2, 10, 15, 3), nrow = 2, dimnames =
      list(c("Dizygotic", "Monozygotic"), c("Convicted", "Not convicted")))

  model <- fisher.test(Convictions, conf.int = FALSE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Fisher's r x c tests work", {
  correct <- test_results[["fisher_test_r_by_c"]]

  Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
    dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
      satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))

  model <- fisher.test(Job)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("Fisher's tests with simulated p-values work", {
  correct <- test_results[["fisher_test_simulated_p"]]

  Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
    dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
      satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))

  model <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .02)
})

test_that("Fisher's hybrid tests work", {
  correct <- test_results[["fisher_test_hybrid"]]

  MP6 <- rbind(
    c(1,2,2,1,1,0,1),
    c(2,0,0,2,3,0,0),
    c(0,1,1,1,2,7,3),
    c(1,1,2,0,0,0,1),
    c(0,1,1,1,1,0,0)
  )

  model <- fisher.test(MP6, hybrid = TRUE)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})