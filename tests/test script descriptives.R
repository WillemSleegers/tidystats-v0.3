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

# Add to the list
starwars %>%
  descriptives(height) %>%
  add_descriptives(descriptives, "height") -> descriptives

# 1 variable, 1 group
View(descriptives(starwars, height, species))

# Add to the list
starwars %>%
  descriptives(height, species) %>%
  add_descriptives(descriptives, "height_by_species") -> descriptives

# 1 variable, 2 groups
View(descriptives(starwars, height, species, gender))

# Add to the list
starwars %>%
  descriptives(height, species, gender) %>%
  add_descriptives(descriptives, "height_by_species_by_gender") -> descriptives

# Test descriptives: count data ---------------------------------------------------------------

# 1 group
View(frequencies(starwars, gender))

# Add to the list
starwars %>%
  frequencies(gender) -> descriptives

# 2 groups
View(frequencies(starwars, gender, species))

# Save results --------------------------------------------------------------------------------

# Convert to data frame
descriptives_list_to_df(descriptives)

# Save to disk
write_descriptives(descriptives, "tests/descriptives.csv")
