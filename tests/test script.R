
# Setup -------------------------------------------------------------------

# Load packages
library(devtools)

install_github("willemsleegers/tidystats")
library(tidystats)

# Create empty tidy stats data frame
results <- new_stats_data_frame()

# Test independent t-test -------------------------------------------------

# Use the 'sleep' data set
sleep

# Run model and add output to results
model1 <- t.test(extra ~ group, data = sleep)

# Add model output to results
results <- add_stats(results, model1, identifier = "M1", type = "hypothesis",
  description = "Test whether an independent t-test works.")

# Test paired t-test ------------------------------------------------------

# Use the sleep data set
sleep

# Run model
model2 <- t.test(extra ~ group, data = sleep, paired = T)

# Add model output to results
results <- add_stats(results, model2, identifier = "M2", type = "hypothesis",
  description = "Test whether a paired t-test works.")

# Test correlation --------------------------------------------------------

# Create data
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# Run model (Pearson's product-moment correlation)
model3 <- cor.test(x, y, method = "pearson")

# Add model output to results
results <- add_stats(results, model3, identifier = "M3", type = "hypothesis",
  description = "Test whether a Pearson's product moment correlation works.")

# Run model (Kendall's rank correlation tau)
model4 <- cor.test(x, y, method = "kendall")

# Add model output to results
results <- add_stats(results, model4, identifier = "M4", type = "hypothesis",
  description = "Test whether a Kendall's rank correlation tau works.")

# Run model (Kendall's rank correlation tau)
model5 <- cor.test(x, y, method = "spearman")

# Add model output to results
results <- add_stats(results, model5, identifier = "M5", type = "hypothesis",
  description = "Test whether a Spearman's rank correlation rho works.")

# Test regression ---------------------------------------------------------

# Use the 'attitude' data set
attitude

# Run model with 1 predictor
model6 <- lm(rating ~ complaints, data = attitude)

# Add model output to results
results <- add_stats(results, model6, identifier = "M6", type = "hypothesis",
  description = "Test whether regression with 1 predictor works.")

# Run model with 2 predictors
model7 <- lm(rating ~ complaints + critical, data = attitude)

# Add model output to results
results <- add_stats(results, model7, identifier = "M7", type = "hypothesis",
  description = "Test whether regression with 2 predictors works.")

# Run model with interaction effect
model8 <- lm(rating ~ complaints * critical, data = attitude)

# Add model output to results
results <- add_stats(results, model8, identifier = "M8", type = "hypothesis",
  description = "Test whether regression with an interaction effect works.")

# Test ANOVA --------------------------------------------------------------

# Use the 'affect' data from the psych package
library(psych)
affect

# Convert the 'Film' variable to a factor
affect$Film <- factor(affect$Film)

# Run model with Film as a factor and PA2 as DV
model9 <- aov(PA2 ~ Film, data = affect)

# Add model output to results
results <- add_stats(results, model9, identifier = "M9", type = "hypothesis",
  description = "Test whether regression with an interaction effect works.")

# Save to file --------------------------------------------------------------------------------

write.csv(results, file = "tests/results.csv", row.names = FALSE, na = "")
