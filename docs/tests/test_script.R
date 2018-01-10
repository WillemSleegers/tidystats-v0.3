
# Setup ---------------------------------------------------------------------------------------

# Load packages
library(devtools)
document()
install()

# install_github("willemsleegers/tidystats")
library(tidystats)
library(tidyverse)

# Create empty tidy stats data frame
results <- list()
descriptives <- list()

# Set options
options(digits = 7, scipen = 99, dplyr.print_max = 400)

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
results <- add_stats(model3_2, results, identifier = "M3_2", confirmatory = TRUE)

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
model3_2_CIs <- confint(model3_2)
model3_3_CIs <- confint(model3_3)

# Tidy results and add results to existing model
results <- model3_1_CIs %>%
  tidy_stats_confint() %>%
  add_stats_to_model(results, identifier = "M3_1")

results <- model3_2_CIs %>%
  tidy_stats_confint() %>%
  add_stats_to_model(results, identifier = "M3_2")

results <- model3_3_CIs %>%
  tidy_stats_confint() %>%
  add_stats_to_model(results, identifier = "M3_3")

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


# effsize package -----------------------------------------------------------------------------

library(effsize)

?cohen.d

d <- cohen.d(extra ~ group, data = sleep, paired = T)

class(d)

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



# Manually adding model output ----------------------------------------------------------------

x_squared_data <- tibble(
  statistic = c("X-squared", "df", "p"),
  value = c(5.4885, 6, 0.4828),
  method = "Chi-squared test of independence"
)

results <- add_stats(x_squared_data, results, identifier = "x_squared")

some_data <- tibble(
  term = c("group1", "group1", "group2", "group2"),
  statistic = c("t", "p", "t", "p"),
  value = c(5.4885, 0.04, 4.828, 0.06),
  method = "A test"
)

results <- add_stats(some_data, results, identifier = "some_data")

# Report functions ----------------------------------------------------------------------------

report(results, "M1_1")
report(results, "M3_1", term_nr = 2)
report(results, "M3_1", term_nr = 3)
report(results, "x_squared", s = "p")
report(results, "some_data", term = "group1", s = "p")

# Report table functions ----------------------------------------------------------------------

report_table_lm(results, "M3_1")
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"))
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), include_model = FALSE)
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), statistics = c("b", "p"))
report_table_lm(results, "M3_1", terms = "conditionmortality salience", term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"), include_model = FALSE)


# Report single value statistics --------------------------------------------------------------

report_descriptive(results, "D1", var = "", group = "", statistic = "")

# Glmnet package support ----------------------------------------------------------------------

library(glmnet)
data(QuickStartExample)

fit = glmnet(x, y)
plot(fit, label = T)



results$M3_1 %>%
  spread(statistic, value) %>%
  #select(term, b, SE, t, df, p, `F`, `numerator df`, `denominator df`) %>%
  View()



# Describe functions --------------------------------------------------------------------------

# Numeric variables can be described using describe()

# 1 var
results <- cox %>%
  describe(avoidance) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_1", type = "d")

# 1 var, 1 group
results <- cox %>%
  group_by(condition) %>%
  describe(avoidance) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_2", type = "d", statistics = c("n", "M", "SD", "min", "max"))

# 2 vars
results <- cox %>%
  describe(avoidance, anxiety) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_3", type = "d", statistics = c("n", "M", "SD", "min", "max"))

# 2 vars, 1 group
results <- cox %>%
  group_by(condition) %>%
  describe(avoidance, anxiety) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_4", type = "d", statistics = c("n", "M", "SD", "min", "max"))

# 1 var, 2 groups
results <- cox %>%
  group_by(condition, sex) %>%
  describe(avoidance) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_5", type = "d", statistics = c("n", "M", "SD", "min", "max"))

# 2 vars, 2 groups
results <- cox %>%
  group_by(condition, sex) %>%
  describe(avoidance, anxiety) %>%
  tidy_describe() %>%
  add_stats(results, identifier = "D1_6", type = "d", statistics = c("n", "M", "SD", "min", "max"))

# Non-numeric variables can be totaled using total()

# 1 var
cox %>%
  total(condition) %>%
  tidy_total() %>%
  add_stats(results, identifier = "D2_1", type = "d")

# 2 vars
cox %>%
  total(condition, sex) %>%
  tidy_total() %>%
  add_stats(results, identifier = "D2_2", type = "d")

# 1 var, 1 group
cox %>%
  group_by(sex) %>%
  total(condition) %>%
  tidy_total() %>%
  add_stats(results, identifier = "D2_3", type = "d")

# 2 vars, 1 group
cox$age_group <- if_else(cox$age > 20, "old", "young")

cox %>%
  group_by(sex) %>%
  total(condition, age_group) %>%
  tidy_total() %>%
  add_stats(results, identifier = "D2_4", type = "d")

# 2 vars, 2 groups
cox$anxious <- if_else(cox$anxiety > 3.25, "anxious", "non-anxious")

cox %>%
  group_by(sex, age_group) %>%
  total(condition, anxious) %>%
  tidy_total() %>%
  add_stats(results, identifier = "D2_5", type = "d")

# Report descriptives -------------------------------------------------------------------------

# Generic function
D(descriptives, "D1", "avoidance", statistic = "M")
D(descriptives, "D1", "avoidance", statistic = "SD")
D(descriptives, "D1", "avoidance", statistic = "n")

# Specific functions
M(descriptives, "D1", "avoidance")
SD(descriptives, "D1", "avoidance")
