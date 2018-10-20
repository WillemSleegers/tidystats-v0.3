context("test-glm")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("generalized linear models with a poisson family work", {
  correct <- test_results[["glm_poisson"]]

  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  d.AD <- data.frame(treatment, outcome, counts)

  model <- glm(counts ~ outcome + treatment, family = poisson())
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("generalized linear models with a gaussian family work", {
  correct <- test_results[["glm_gaussian"]]

  utils::data(anorexia, package = "MASS")
  model <- glm(Postwt ~ Prewt + Treat + offset(Prewt), data = anorexia)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("generalized linear models with a gamma family work", {
  correct <- test_results[["glm_gamma"]]

  clotting <- data_frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12)
  )

  model <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})
