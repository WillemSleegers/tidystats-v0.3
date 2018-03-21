
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

# Descriptives --------------------------------------------------------------------------------

# count_data() --------------------------------------------------------------------------------

# 1 categorical variable
count_data(cox, condition)

# 2 categorical variables
count_data(cox, condition, sex)

# 1 categorical variable, 1 grouping variable
cox %>%
  group_by(sex) %>%
  count_data(condition)

# tidy_count_data() ---------------------------------------------------------------------------

# 1 categorical variable
count_data(cox, condition) %>%
  tidy_count_data()

# 2 categorical variables
count_data(cox, condition, sex) %>%
  tidy_count_data()

# 1 categorical variable, 1 grouping variable
cox %>%
  group_by(sex) %>%
  count_data(condition) %>%
  tidy_count_data()

# describe_data() -----------------------------------------------------------------------------

# 1 variable
describe_data(cox, avoidance)

# 2 variables
describe_data(cox, avoidance, anxiety)

# 1 variable, 1 group
cox %>%
 group_by(condition) %>%
 describe_data(avoidance)

# 2 variables, 1 group
cox %>%
 group_by(condition) %>%
 describe_data(avoidance, anxiety)

# 1 variable, 2 groups
cox %>%
 group_by(condition, sex) %>%
 describe_data(avoidance)

# 2 variables, 2 groups
cox %>%
 group_by(condition, sex) %>%
 describe_data(avoidance, anxiety)

# tidy_describe_data() ------------------------------------------------------------------------

# 1 variable
describe_data(cox, avoidance) %>%
  tidy_describe_data()

# 2 variables
describe_data(cox, avoidance, anxiety) %>%
  tidy_describe_data()

# 1 variable, 1 group
cox %>%
  group_by(condition) %>%
  describe_data(avoidance) %>%
  tidy_describe_data()

# 2 variables, 1 group
cox %>%
  group_by(condition) %>%
  describe_data(avoidance, anxiety) %>%
  tidy_describe_data()

# 1 variable, 2 groups
cox %>%
  group_by(condition, sex) %>%
  describe_data(avoidance) %>%
  tidy_describe_data()

# 2 variables, 2 groups
cox %>%
  group_by(condition, sex) %>%
  describe_data(avoidance, anxiety) %>%
  tidy_describe_data()

# add_stats(): Descriptives -------------------------------------------------------------------

# Count data

# 1 var
results <- cox %>%
  count_data(condition) %>%
  tidy_count_data() %>%
  add_stats(results, identifier = "D1_condition", type = "d")

# 2 vars
results <- cox %>%
  count_data(condition, sex) %>%
  tidy_count_data() %>%
  add_stats(results, identifier = "D2_condition_sex", type = "d")

# 1 var, 1 group
results <- cox %>%
  group_by(sex) %>%
  count_data(condition) %>%
  tidy_count_data() %>%
  add_stats(results, identifier = "D3_condition_by_sex", type = "d")

# Non-count data

# 1 var
results <- cox %>%
  describe_data(avoidance) %>%
  tidy_describe_data() %>%
  add_stats(results, identifier = "D4_avoidance", type = "d")

# 2 vars
results <- cox %>%
  describe_data(avoidance, anxiety) %>%
  tidy_describe_data() %>%
  add_stats(results, identifier = "D5_avoidance_anxiety", type = "d",
            statistics = c("n", "M", "SD", "min", "max"))

# 1 var, 1 group
results <- cox %>%
  group_by(condition) %>%
  describe_data(avoidance) %>%
  tidy_describe_data() %>%
  add_stats(results, identifier = "D6_avoidance_by_condition", type = "d",
            statistics = c("n", "M", "SD", "min", "max"))

# 2 vars, 2 groups
results <- cox %>%
  group_by(condition, sex) %>%
  describe_data(avoidance, anxiety) %>%
  tidy_describe_data() %>%
  add_stats(results, identifier = "D7_avoidance_anxiety_by_condition_sex", type = "d",
            statistics = c("n", "M", "SD", "min", "max"))

# Analysis: t-tests ---------------------------------------------------------------------------

# Run t-tests
t_test_one_sample <- t.test(cox$call_parent)
t_test_two_sample <- t.test(call_parent ~ condition, data = cox, var.equal = TRUE)
t_test_welch <- t.test(call_parent ~ condition, data = cox, var.equal = FALSE)
t_test_paired <- t.test(cox$affect_positive, cox$affect_negative, paired = TRUE)

# Tidy results
tidy_stats(t_test_one_sample)
tidy_stats(t_test_two_sample)
tidy_stats(t_test_welch)
tidy_stats(t_test_paired)

# Add stats
results <- add_stats(t_test_one_sample, results, identifier = "t_test_one_sample",
                     statistics = c("t", "df", "p"), type = "h", confirmatory = TRUE)
results <- add_stats(t_test_one_sample, results, identifier = "t_test_two_sample")
results <- add_stats(t_test_one_sample, results, identifier = "t_test_welch")
results <- add_stats(t_test_one_sample, results, identifier = "t_test_paired")

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
model3_2_CIs <- confint(model3_2, level = .90)
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

# Save to file --------------------------------------------------------------------------------

write_stats(results, path = "docs/tests/results.csv")

# Report functions ----------------------------------------------------------------------------

# Set the tidystats list in options
# We cannot do this already in the beginning of the script since it copies the list, meaning it
# does not contain any results yet
options(tidystats_list = results)

# Report: Descriptives ------------------------------------------------------------------------

report("D1_condition", statistic = "M") # Error
report("D1_condition", statistic = "n") # Error
report("D1_condition", statistic = "n", group = "dental pain") # Error

report("D4_avoidance", statistic = "M")
M("D4_avoidance")
SD("D4_avoidance")

report("D5_avoidance_anxiety", var = "avoidance", statistic = "M")
M("D5_avoidance_anxiety", var = "avoidance")

report("D6_avoidance_by_condition", var = "avoidance", group = "dental pain", statistic = "M")
M("D6_avoidance_by_condition", var = "avoidance", group = "dental pain")

# Report: t-tests -----------------------------------------------------------------------------

report("M1_1")
report("M1_1", statistic = "p")
report("M1_2")
report("M1_2", statistic = "p")
report("M1_3")
report("M1_4")

# Correlation
report("M2_1")
report("M2_1", statistic = "cor")
report("M2_2")
report("M2_2", statistic = "tau")
report("M2_3")
report("M2_3", statistic = "rho")

# Report regression results
report("M3_1", term = "conditionmortality salience")
report("M3_1", term_nr = 2)
report("M3_1", term = "(Model)")
report("M3_1", term_nr = 3)


report("M3_2", term_nr = 3)

# ANOVA
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


# Report table functions ----------------------------------------------------------------------

report_table_lm(results, "M3_1")
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"))
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), include_model = FALSE)
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), statistics = c("b", "p"))
report_table_lm(results, "M3_1", terms = "conditionmortality salience", term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"), include_model = FALSE)

