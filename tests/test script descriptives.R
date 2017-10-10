# Setup ---------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidystats)

# Create empty list to store descriptives in
results <- list()

# Test data
glimpse(mpg)

# Set options
options(digits = 2)

# Test descriptives: continuous data ----------------------------------------------------------

# 1 variable, no groups
mpg %>%
  describe(cty)

mpg %>%
  describe(cty) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "cty", statistics = c("n", "M", "SD"), type = "d",
            confirmatory = T, notes = "This is a test.")

# 1 variable, 1 group
mpg %>%
  group_by(year) %>%
  describe(cyl)

results <- mpg %>%
  group_by(year) %>%
  describe(cty) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "cty_by_year")

# 1 variable, 2 groups
mpg %>%
  group_by(year, class) %>%
  describe(cyl) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "cty_by_year_class")

# 1 variable, 3 groups
mpg %>%
  group_by(year, class, cyl) %>%
  describe(cty) %>%
  tidy_descriptives() %>%
  add_stats(results, identifier = "cty_by_year_class_cyl")

# Test descriptives: count data ---------------------------------------------------------------

# 1 group
mpg %>%
  describe(class)

# 2 groups
mpg %>%
  group_by(cyl) %>%
  describe(class)

# 3 groups
mpg %>%
  group_by(cyl, fl) %>%
  describe(class)

# Save results --------------------------------------------------------------------------------

# Convert to data frame
View(descriptives_list_to_df(results))

# Save to disk
write_descriptives(results, "tests/descriptives.csv")
