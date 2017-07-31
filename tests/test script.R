
# Setup -------------------------------------------------------------------

# Load packages
library(devtools)

install_github("willemsleegers/tidystats")
library(tidystats)

# Create empty tidy stats data frame
results <- new_stats_data_frame()

# Test independent t-test -------------------------------------------------

# Use the sleep data set
sleep

# Run model
model1 <- t.test(extra ~ group, data = sleep)

# Add model output to results
results <- addstats(results, model1, identifier = "M1", type = "hypothesis", description = "Test whether an independent t-test works.")

# Test paired t-test ------------------------------------------------------

# Use the sleep data set
sleep

# Run model
model2 <- t.test(extra ~ group, data = sleep, paired = T)

# Add model output to results
results <- addstats(results, model2, identifier = "M2", type = "hypothesis", description = "Test whether a paired t-test works.")

# Test correlation --------------------------------------------------------

# Create data
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# Run model (Pearson's product-moment correlation)
model3 <- cor.test(x, y)

# Add model output to results
results <- addstats(results, model3, identifier = "M3", type = "hypothesis", description = "Test whether a Pearson's product moment correlation works.")

# Run model (Kendall's rank correlation tau)
model3 <- cor.test(x, y, method = "kendall")

# Add model output to results
results <- addstats(results, model3, identifier = "M4", type = "hypothesis", description = "Test whether a Kendall's rank correlation tau works.")
# Error: object 'parameter' not found
