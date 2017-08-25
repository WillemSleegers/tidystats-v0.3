# Setup ---------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidystats)

# Create empty list to store descriptives in
results <- list()

# Test data: starwars
glimpse(starwars)

# Test descriptives: continuous data ----------------------------------------------------------

# 1 variable, no groups
View(descriptives(starwars, height))

results <- starwars %>%
  descriptives(height) %>%
  add_descriptives(results, "D1")

# 1 variable, no groups, select subset of values
results <- starwars %>%
  descriptives(height) %>%
  add_descriptives(results, "D1_1", subset = c("n", "M", "SD"))

# 1 variable, 1 group
View(descriptives(starwars, height, species))

results <- starwars %>%
  descriptives(height, species) %>%
  add_descriptives(results, "D2")

# 1 variable, 2 groups
View(descriptives(starwars, height, species, gender))

results <- starwars %>%
  descriptives(height, species, gender) %>%
  add_descriptives(results, "D3")

# Test descriptives: count data ---------------------------------------------------------------

# 1 group
View(frequencies(starwars, gender))

results <- starwars %>%
  frequencies(gender) %>%
  add_descriptives(results, "D4")

# 2 groups
View(frequencies(starwars, gender, species))

results <- starwars %>%
  frequencies(gender, species) %>%
  add_descriptives(results, "D5")

# Save results --------------------------------------------------------------------------------

# Convert to data frame
View(descriptives_list_to_df(results))

# Save to disk
write_descriptives(descriptives, "tests/descriptives.csv")
