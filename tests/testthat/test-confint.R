context("test-glm")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("confint 95% works", {
  correct <- test_results[["confint_lm_95"]]

  model <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
  model2 <- confint(model, level = .95)
  output <- tidy_stats.confint(model2)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("confint 90% works", {
  correct <- test_results[["confint_lm_90"]]

  model <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
  model2 <- confint(model, level = .90)
  output <- tidy_stats.confint(model2)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("adding confint to lm works", {
  correct <- test_results[["lm_confint"]]

  model <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
  model2 <- confint(model, level = .95)

  output <- add_stats(list(), model)
  output <- add_stats_to_model(output, model2, identifier = "model",
    class = "confint")
  output <- output$model

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("profile likelihood confint works", {
  correct <- test_results[["confint_glm_profile_likelihood"]]

  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
  model <- glm(counts ~ outcome + treatment, family = poisson())
  model2 <- confint(model)

  output <- tidy_stats.confint(model2)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("asymptotic normality confint works", {
  correct <- test_results[["confint_glm_asymptotic_normality"]]

  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
  model <- glm(counts ~ outcome + treatment, family = poisson())
  model2 <- confint.default(model)

  output <- tidy_stats.confint(model2)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})