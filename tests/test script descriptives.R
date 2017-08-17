# Setup ---------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidystats)

# Create empty list to store descriptives in
descriptives <- list()

# Test descriptives ---------------------------------------------------------------------------

# Sleep data
glimpse(sleep)

# 1 variable, no groups
describe(sleep, DV = "extra")

# Add to the list
descriptives <- sleep %>%
  describe(DV = "extra") %>%
  add_descriptives(descriptives)

# 1 variable, 1 group
describe(sleep, variable = "extra", group = "group")

# Add to the list
descriptives <- sleep %>%
  describe(variable = "extra", group = "group") %>%
  add_descriptives(descriptives)

# Save results --------------------------------------------------------------------------------

# Convert to data frame
descriptives_list_to_df(descriptives)

# Save to disk
write_descriptives(descriptives, "tests/descriptives.csv")
