context("test-lmerTest")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("lmerTest's linear mixed models work", {
  correct <- test_results[["lmerTest_lme"]]

  model <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("lmerTest's linear mixed models with uncorrelated terms work", {
  correct <- test_results[["lmerTest_lme_uncorrelated"]]

  model <- lmerTest::lmer(Reaction ~ Days + (Days || Subject), lme4::sleepstudy)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("lmerTest's linear mixed models with dummies work", {
  skip_if_not_installed("nlme")
  correct <- test_results[["lmerTest_lme_dummies"]]
  data(Orthodont, package = "nlme")
  Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
  Orthodont$nsexage <- with(Orthodont, nsex * age)
  model <- lmerTest::lmer(distance ~ age + (age|Subject) + (0 + nsex|Subject) +
      (0 + nsexage|Subject), data = Orthodont)

  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
