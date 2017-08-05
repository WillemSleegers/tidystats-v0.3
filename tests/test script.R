
# Setup -------------------------------------------------------------------

# Load packages
# library(devtools)

# install_github("willemsleegers/tidystats")
library(tidystats)
library(dplyr)

# Create empty tidy stats data frame
results <- list()

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

# Test regression ---------------------------------------------------------

# Use the 'attitude' data set
attitude

# Model with 1 predictor
model3_1 <- lm(rating ~ complaints, data = attitude)
results <- add_stats(results, model3_1, identifier = "M3_1", type = "hypothesis")

# Model with 2 predictors
model3_2 <- lm(rating ~ complaints + critical, data = attitude)
results <- add_stats(results, model3_2, identifier = "M3_2", type = "hypothesis")

# Model with interaction effect
model3_3 <- lm(rating ~ complaints * critical, data = attitude)
results <- add_stats(results, model3_2, identifier = "M3_3", type = "hypothesis")

# # Test ANOVA --------------------------------------------------------------

# Use the 'affect' data from the psych package
library(psych)
affect

# Convert variables to factors
affect$Film <- factor(affect$Film)
affect$Study <- factor(affect$Study)

# One-way ANOVA
model4_1 <- aov(PA2 ~ Film, data = affect)
results <- add_stats(results, model4_1, identifier = "M4_1", type = "hypothesis")

# Two-way ANOVA
model4_2 <- aov(PA2 ~ Film + Study, data = affect)
results <- add_stats(results, model4_2, identifier = "M4_2", type = "hypothesis")

# Two-way ANOVA with interaction
model4_3 <- aov(PA2 ~ Film*Study, data = affect)
results <- add_stats(results, model4_3, identifier = "M4_3", type = "hypothesis") # Breaks down

# Convert to data frame -----------------------------------------------------------------------

df <- stats_list_to_df(results)
View(df)

# Save to file --------------------------------------------------------------------------------

write_stats(df, file = "tests/results.csv")
