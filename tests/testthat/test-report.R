context("test-report")

# Read test results
test_results <- read_stats("test_results.csv")

# Set this list as the default tidystats list
options(tidystats_list = test_results)

test_that("reporting paired t-tests work", {
  correct <- "*t*(199) = 14.18, *p* < .001, 95% CI [7.12, 9.42]"

  output <- report("t_test_paired")

  testthat::expect_equal(correct, output)
})

test_that("reporting linear regression coefficients work", {
  correct <- "*b* = 2.77, *SE* = 2.12, *t*(198) = 1.30, *p* = .19"

  output <- report("lm_simple", term_nr = 2)

  testthat::expect_equal(correct, output)
})

test_that("reporting linear regression model fit works", {
  correct <- "adjusted *R²* = 0.0035, *F*(1, 198) = 1.70, *p* = .19"

  output <- report("lm_simple", group = "model")

  testthat::expect_equal(correct, output)
})