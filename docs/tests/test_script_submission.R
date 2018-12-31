
# Testing -----------------------------------------------------------------

# Test all tests
devtools::test()

# Test specific tests
library(testthat)
test_results <- read_stats("tests/testthat/test_results.csv")
test_file("tests/testthat/test-psych.R")

# README ------------------------------------------------------------------

library(knitr)
knit("README.Rmd")

# CRAN submission ---------------------------------------------------------

# Check examples
devtools::run_examples()

# Check package
devtools::check()
devtools::check(args = c('--run-donttest'))

# run R CMD check on CRAN’s servers
devtools::check_win_release()

# Build tar
build()