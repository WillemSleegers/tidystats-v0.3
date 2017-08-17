# Setup ---------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidystats)

# Create empty list to store descriptives in
descriptives <- list()

# Load example data
data <- sleep

# Sample size ---------------------------------------------------------------------------------

# Total N
descriptives <- sleep %>%
  summarize(
    N = n()
  ) %>%
  add_descriptives(descriptives, identifier = "total_N")

# N per group
descriptives <- sleep %>%
  group_by(group) %>%
  summarize(
    N = n()
  ) %>%
  add_descriptives(descriptives, identifier = "total_N", group = "group")

# Descriptives --------------------------------------------------------------------------------

descriptives <- sleep %>%
  group_by(group) %>%
  summarize(
    extra_mean = mean(extra),
    extra_sd = sd(extra)
  ) %>%
  add_descriptives(descriptives, identifier = "extra", group = "group")

