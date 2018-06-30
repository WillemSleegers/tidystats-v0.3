
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)

# Load tidystats results
results <- read_stats("docs/tests/results.csv")

# Report functions --------------------------------------------------------

# Set the tidystats list in options
options(tidystats_list = results)

# Report: t-tests ---------------------------------------------------------

# Complete output
report("t_test_one_sample")
report("t_test_two_sample")
report("t_test_welch")
report("t_test_paired")

# Single statistic output
report("t_test_one_sample", statistic = "t")
report("t_test_one_sample", statistic = "df")
report("t_test_one_sample", statistic = "p")

# Report: correlations ----------------------------------------------------

# Complete output
report("correlation_pearson")
report("correlation_spearman")
report("correlation_kendall")

# Single statistic output
report("correlation_pearson", statistic = "cor")
report("correlation_spearman", statistic = "rho")
report("correlation_pearson", statistic = "95% CI lower")

# Report: chi-square ------------------------------------------------------

report("chi_square")
report("chi_square", statistic = "X-squared")

# Report: regression ------------------------------------------------------

# Complete output
report("lm_parent_condition", group = "model")
report("lm_parent_condition", term = "conditionmortality salience")
report("lm_parent_condition", term_nr = 2)

# Single statistic output
report("lm_parent_condition", term_nr = 2, statistic = "b")
report("lm_parent_condition", term_nr = 2, statistic = "df")
report("lm_parent_condition", term_nr = 2, statistic = "p")

# ANOVA -------------------------------------------------------------------

# Complete output
report("aov_parent_condition", term = "condition")
report("aov_parent_condition", term_nr = 1)
report("aov_parent_condition", statistic = "F")
report("aov_parent_condition", statistic = "SS")
report("aov_parent_condition", term = "condition", statistic = "SS")


report("aov_parent_condition", term = "Residuals")
report("aov_parent_condition", term_nr = 2)

report("aov_parent_affect", term = "affect")


# Inspect model -----------------------------------------------------------

inspect_model(results)

# Report table functions ----------------------------------------------------------------------

report_table_lm(results, "M3_1")
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"))
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), include_model = FALSE)
report_table_lm(results, "M3_1", term_labels = c("Intercept", "Condition"), statistics = c("b", "p"))
report_table_lm(results, "M3_1", terms = "conditionmortality salience", term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"))
report_table_lm(results, "M3_1", term_nrs = 2, term_labels = c("Condition"), include_model = FALSE)



# Correlation matrix table --------------------------------------------------------------------

library(psych)
library(Hmisc)

correlation_results <- rcorr(as.matrix(select(affect, ext, neur, imp, soc, lie)))
correlations <- correlation_results$r
p_values <- correlation_results$P

# Report: Descriptives ----------------------------------------------------

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
