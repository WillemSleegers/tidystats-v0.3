
# Setup ---------------------------------------------------------------------------------------

# Load packages
library(devtools)
install()

# install_github("willemsleegers/tidystats")
library(tidystats)
library(tidyverse)

# Create empty tidy stats data frame
results <- list()

# Set options
options(digits = 7, scipen = 99)

# Test t-tests --------------------------------------------------------------------------------

# Use the 'cox' data set
cox

# One sample t-test
model1_1 <- t.test(cox$call_parent)
model1_1
results <- add_stats(model1_1, results, identifier = "M1_1", statistics = c("t", "df", "p"),
                     type = "h", confirmatory = TRUE)

# Two sample t-test
model1_2 <- t.test(call_parent ~ condition, data = cox, var.equal = TRUE)
model1_2
results <- add_stats(model1_2, results, identifier = "M1_2")

# Welch's Two sample t-test
model1_3 <- t.test(call_parent ~ condition, data = cox, var.equal = FALSE)
model1_3
results <- add_stats(model1_3, results, identifier = "M1_3")

# Paired t-test
model1_4 <- t.test(cox$affect_positive, cox$affect_negative, data = cox, paired = TRUE)
model1_4
results <- add_stats(model1_4, results, identifier = "M1_4")

# Test correlation --------------------------------------------------------

# Pearson's product-moment correlation
model2_1 <- cor.test(cox$call_parent, cox$anxiety, method = "pearson")
model2_1
results <- add_stats(model2_1, results, identifier = "M2_1")

# Kendall's rank correlation tau
model2_2 <- cor.test(cox$call_parent, cox$anxiety, method = "kendall")
model2_2
results <- add_stats(model2_2, results, identifier = "M2_2")

# Spearman's rank correlation rho
model2_3 <- cor.test(cox$call_parent, cox$anxiety, method = "spearman")
model2_3
results <- add_stats(model2_3, results, identifier = "M2_3")

# Test regression ---------------------------------------------------------

# Model with 1 predictor
model3_1 <- lm(call_parent ~ condition, data = cox)
summary(model3_1)
results <- add_stats(model3_1, results, identifier = "M3_1")

# Model with 2 predictors
model3_2 <- lm(call_parent ~ condition + anxiety, data = cox)
summary(model3_2)
results <- add_stats(model3_2, results, identifier = "M3_2")

# Model with interaction effect
model3_3 <- lm(call_parent ~ condition * anxiety, data = cox)
summary(model3_3)
results <- add_stats(model3_3, results, identifier = "M3_3")

# Test ANOVA --------------------------------------------------------------

# Convert variables to factors
cox$condition <- factor(cox$condition)

# One-way ANOVA
model4_1 <- aov(call_parent ~ condition, data = cox)
summary(model4_1)
results <- add_stats(model4_1, results, identifier = "M4_1")

# Two-way ANOVA
model4_2 <- aov(call_parent ~ condition + sex, data = cox)
summary(model4_2)
results <- add_stats(model4_2, results, identifier = "M4_2")

# Two-way ANOVA with interaction
model4_3 <- aov(call_parent ~ condition * sex, data = cox)
summary(model4_3)
results <- add_stats(model4_3, results, identifier = "M4_3")

# ANCOVA
model4_4 <- aov(call_parent ~ condition + affect_negative, data = cox)
summary(model4_4)
results <- add_stats(model4_4, results, identifier = "M4_4")

# Prepare for repeated measures ANOVAs
cox %>%
  select(ID, condition, anxiety, affect_positive, affect_negative) %>%
  gather("affect", "score", affect_positive, affect_negative) %>%
  arrange(ID) -> cox_long

cox_long$ID <- factor(cox_long$ID)
cox_long$affect <- factor(cox_long$affect)

# One within subject factor
model4_5 <- aov(score ~ affect + Error(ID/affect), data = cox_long)
summary(model4_5)
results <- add_stats(model4_5, results, identifier = "M4_5")

# Mixed design
model4_6 <- aov(score ~ condition * affect + Error(ID/affect) + condition, data = cox_long)
summary(model4_6)
results <- add_stats(model4_6, results, identifier = "M4_6")

# ANCOVA with within subject factor
model4_7 <- aov(score ~ affect + anxiety + Error(ID/affect) + anxiety, data = cox_long)
summary(model4_7)
results <- add_stats(model4_7, results, identifier = "M4_7")

# Add results to existing model ---------------------------------------------------------------

# Calculate confidence intervals
model3_1_CIs <- confint(model3_1)

# Tidy results and add results to existing model
results <- model3_1_CIs %>%
  tidy_stats_confint() %>%
  add_stats_to_model(results, identifier = "M3_1")

# Convert to data frame -----------------------------------------------------------------------

df <- stats_list_to_df(results)
View(df)

# Show output of 1 model (wide) ---------------------------------------------------------------

results$M4_6 %>%
  spread(statistic, value) %>%
  View()

# Save to file --------------------------------------------------------------------------------

write_stats(results, path = "tests/results.csv")

# Output functions ----------------------------------------------------------------------------

report(results, "M1_1")
report(results, "M1_1", statistic = "p")
report(results, "M1_2")
report(results, "M1_2", statistic = "p")
report(results, "M1_3")
report(results, "M1_4")
report(results, "M2_1")
report(results, "M2_1", statistic = "cor")
report(results, "M2_2")
report(results, "M2_2", statistic = "tau")
report(results, "M2_3")
report(results, "M2_3", statistic = "rho")
report(results, "M3_1", term = "conditionmortality salience")
report(results, "M3_1", term_nr = 2)
report(results, "M3_1", term = "(Model)")
report(results, "M3_1", term_nr = 3)
report(results, "M4_1", term = "condition")
report(results, "M4_1", term = "condition", statistic = "F")
report(results, "M4_1", term = "Residuals", statistic = "df")
report(results, "M4_3", term = "condition:sex")
report(results, "M4_6", term = "condition")
report(results, "M4_6", term = "affect")
report(results, "M4_6", term = "condition:affect")
report(results, "M4_7", term = "anxiety")
report(results, "M4_7", term = "affect")

# Unfinished statistical models ---------------------------------------------------------------

# Multilevel models ---------------------------------------------------------------------------

# Use the politeness data from http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf
politeness <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

library(lme4)
# library(lmerTest)

# 1 fixed effect
model5_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data = politeness, REML = F)
results <- add_stats(model5_1, results, identifier = "M5_1", type = "hypothesis")

# 2 fixed effects
model5_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data = politeness, REML = F)
results <- add_stats(model5_2, results, identifier = "M5_2", type = "hypothesis")

# Random slope
model5_3 = lmer(frequency ~ attitude + gender + (1+attitude|subject) + (1+attitude|scenario),
                        data=politeness)
results <- add_stats(model5_3, results, identifier = "M5_3", type = "hypothesis")


# Model comparison with anova() ---------------------------------------------------------------

anova(model3_1)

anova(model3_1, model3_2)

anova(model5_1)

anova(model5_1, model5_2)


# Logistic regression -------------------------------------------------------------------------

data <- data_frame(
  DV = sample(c(0, 1), 20, replace = TRUE),
  IV1 = rnorm(20),
  IV2 = rnorm(20)
)

model6_1 <- glm(DV ~ IV1, data = data, family = binomial)

# MANOVA --------------------------------------------------------------------------------------

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species*Petal.Width , data = iris), test = "Roy")

# Reliability ---------------------------------------------------------------------------------

# Use the 'bfi' data from the psych package
library(psych)
bfi

# Cronbach's alpha
bfi %>%
  select(A1, A2, A3, A4, A5) %>%
  alpha(check.keys = TRUE) -> model8_1



# Mark’s issue --------------------------------------------------------------------------------

results <- aov(lm(extra ~ group, data = sleep)) %>%
  add_stats(results)

results <- t.test(extra ~ group, data = sleep) %>%
  add_stats(results)

extra <- tibble(
  term_nr = 1,
  statistic = "test",
  value = 1.0
)

extra2 <- tibble(
  statistic = "test",
  value = 1.0
)

add_stats_to_model(extra, results, identifier = "M1")
add_stats_to_model(extra2, results, identifier = "M2")

# Requirements should be term or term_nr, statistic, value
# It should also automatically adopt the method (and the term or term_nr, depending on what was provided)
