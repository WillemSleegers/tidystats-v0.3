context("test-lme4")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("lme4's linear mixed models work", {
  correct <- test_results[["lme4_lme"]]

  model <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("lme4's linear mixed models with uncorrelated terms work", {
  correct <- test_results[["lme4_lme_uncorrelated"]]

  model <- lme4::lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("lme4's linear mixed models with dummies work", {
  correct <- test_results[["lme4_lme_dummies"]]

  data(Orthodont, package = "nlme")
  Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
  Orthodont$nsexage <- with(Orthodont, nsex * age)
  model <- lme4::lmer(distance ~ age + (age|Subject) + (0 + nsex|Subject) +
      (0 + nsexage|Subject), data = Orthodont)

  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
