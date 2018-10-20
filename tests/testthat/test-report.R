context("test-report")

# Read test results
test_results <- read_stats("test_results.csv")

# Set this list as the default tidystats list
options(tidystats_list = test_results)

test_that("reporting paired t-tests work", {
  correct <- "*t*(199) = 14.18, *p* < .001, 95% CI [7.12, 9.42]"

  output <- report("t_test_paired")

  testthat::expect_match(output, correct)
})
