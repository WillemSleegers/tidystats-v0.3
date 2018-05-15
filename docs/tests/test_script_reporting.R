
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
