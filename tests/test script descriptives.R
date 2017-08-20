# Setup ---------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidystats)

# Create empty list to store descriptives in
descriptives <- list()

# Test data: starwars
glimpse(starwars)

# Test descriptives: continuous data ----------------------------------------------------------

# 1 variable, no groups
View(descriptives(starwars, height))

starwars %>%
  descriptives(height) %>%
  add_descriptives(descriptives, "D1") -> descriptives

# 1 variable, 1 group
View(descriptives(starwars, height, species))

starwars %>%
  descriptives(height, species) %>%
  add_descriptives(descriptives, "D2") -> descriptives

# 1 variable, 2 groups
View(descriptives(starwars, height, species, gender))

starwars %>%
  descriptives(height, species, gender) %>%
  add_descriptives(descriptives, "D3") -> descriptives

# Test descriptives: count data ---------------------------------------------------------------

# 1 group
View(frequencies(starwars, gender))

starwars %>%
  frequencies(gender) %>%
  add_descriptives(descriptives, "D4")

# 2 groups
View(frequencies(starwars, gender, species))

starwars %>%
  frequencies(gender, species) %>%
  add_descriptives(descriptives, "D5")

# Save results --------------------------------------------------------------------------------

# Convert to data frame
View(descriptives_list_to_df(descriptives))

# Save to disk
write_descriptives(descriptives, "tests/descriptives.csv")
