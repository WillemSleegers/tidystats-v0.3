context("test-aov")

# Read test results
test_results <- read_stats("test_results.csv")

# Run tests, consisting of the following steps:
# - Retrieve the correct output from the test results file
# - Run the statistical model
# - Tidy the output
# - Compare the tidied output with the (correct) output from the file

test_that("one-way ANOVAs work", {
  correct <- test_results[["aov_one_way"]]

  cox <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  )

  model <- aov(call_parent ~ condition, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("two-way ANOVAs work", {
  correct <- test_results[["aov_two_way"]]

  cox <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  )

  model <- aov(call_parent ~ condition + sex, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("two-way ANOVAs with an interaction work", {
  correct <- test_results[["aov_two_way_interaction"]]

  cox <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  )

  model <- aov(call_parent ~ condition * sex, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("ANCOVAs work", {
  correct <- test_results[["aov_ancova"]]

  cox <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  )

  model <- aov(call_parent ~ condition + affect_negative, data = cox)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .02)
})

test_that("one-way within-subjects ANOVAs work", {
  correct <- test_results[["aov_one_within"]]

  cox_long <-dplyr:: mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  ) %>%
    dplyr::select(ID, condition, anxiety, affect_positive, affect_negative) %>%
    tidyr::gather("affect", "score", affect_positive, affect_negative) %>%
    dplyr::arrange(ID) %>%
    dplyr::mutate(
      ID = factor(ID),
      affect = factor(affect)
    )

  model <- aov(score ~ affect + Error(ID/affect), data = cox_long)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("mixed ANOVAs work", {
  correct <- test_results[["aov_mixed"]]

  cox_long <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  ) %>%
    dplyr::select(ID, condition, anxiety, affect_positive, affect_negative) %>%
    tidyr::gather("affect", "score", affect_positive, affect_negative) %>%
    dplyr::arrange(ID) %>%
    dplyr::mutate(
      ID = factor(ID),
      affect = factor(affect)
    )

  model <- aov(score ~ condition * affect + Error(ID/affect) + condition,
    data = cox_long)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})

test_that("one-way within-subjects ANCOVAs work", {
  correct <- test_results[["aov_ancova_with_within"]]

  cox_long <- dplyr::mutate(cox,
    condition = factor(condition),
    sex = factor(sex)
  ) %>%
    dplyr::select(ID, condition, anxiety, affect_positive, affect_negative) %>%
    tidyr::gather("affect", "score", affect_positive, affect_negative) %>%
    dplyr::arrange(ID) %>%
    dplyr::mutate(
      ID = factor(ID),
      affect = factor(affect)
    )

  model <- aov(score ~ affect + anxiety + Error(ID/affect) +
      anxiety, data = cox_long)
  output <- tidy_stats(model)

  expect_equal(as.data.frame(output), as.data.frame(correct), tolerance = .002)
})