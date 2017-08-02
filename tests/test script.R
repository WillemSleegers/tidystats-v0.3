
# Setup -------------------------------------------------------------------

# Load packages
# library(devtools)

# install_github("willemsleegers/tidystats")
library(tidystats)

# Create empty tidy stats data frame
results <- new_stats_list()

# Test t-tests --------------------------------------------------------------------------------

# Use the 'sleep' data set
sleep

# One sample t-test
model1_1 <- t.test(sleep$extra)
results <- add_stats(results, model1_1, identifier = "M1_1", type = "hypothesis")

# Two sample t-test
model1_2 <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
results <- add_stats(results, model1_2, identifier = "M1_2", type = "hypothesis")

# Welch's Two sample t-test
model1_3 <- t.test(extra ~ group, data = sleep, var.equal = FALSE)
results <- add_stats(results, model1_3, identifier = "M1_3", type = "hypothesis")

# Paired t-test
model1_4 <- t.test(extra ~ group, data = sleep, paired = TRUE)
results <- add_stats(results, model1_4, identifier = "M1_4", type = "hypothesis")

# Test correlation --------------------------------------------------------

# Create data
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

# Pearson's product-moment correlation
model2_1 <- cor.test(x, y, method = "pearson")
results <- add_stats(results, model2_1, identifier = "M2_1", type = "hypothesis")

# Kendall's rank correlation tau
model2_2 <- cor.test(x, y, method = "kendall")
results <- add_stats(results, model2_2, identifier = "M2_2", type = "hypothesis")

# Spearman's rank correlation rho
model2_3 <- cor.test(x, y, method = "spearman")
results <- add_stats(results, model2_3, identifier = "M2_3", type = "hypothesis")

# # Test regression ---------------------------------------------------------
#
# # Use the 'attitude' data set
# attitude
#
# # Run model with 1 predictor
# model6 <- lm(rating ~ complaints, data = attitude)
#
# # Add model output to results
# results <- add_stats(results, model6, identifier = "M6", type = "hypothesis",
#   description = "Test whether regression with 1 predictor works.")
#
# # Run model with 2 predictors
# model7 <- lm(rating ~ complaints + critical, data = attitude)
#
# # Add model output to results
# results <- add_stats(results, model7, identifier = "M7", type = "hypothesis",
#   description = "Test whether regression with 2 predictors works.")
#
# # Run model with interaction effect
# model8 <- lm(rating ~ complaints * critical, data = attitude)
#
# # Add model output to results
# results <- add_stats(results, model8, identifier = "M8", type = "hypothesis",
#   description = "Test whether regression with an interaction effect works.")
#
# # Test ANOVA --------------------------------------------------------------
#
# # Use the 'affect' data from the psych package
# library(psych)
# affect
#
# # Convert the 'Film' variable to a factor
# affect$Film <- factor(affect$Film)
#
# # Run model with Film as a factor and PA2 as DV
# model9 <- aov(PA2 ~ Film, data = affect)
#
# # Add model output to results
# results <- add_stats(results, model9, identifier = "M9", type = "hypothesis",
#   description = "Test whether regression with an interaction effect works.")

# Save to file --------------------------------------------------------------------------------

write_stats(results, file = "tests/results.csv")
