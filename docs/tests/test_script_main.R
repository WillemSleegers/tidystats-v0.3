
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

# Analysis: t.test() --------------------------------------------------------------------------

# Run t-tests
t_test_one_sample <- t.test(cox$call_parent, alternative = "greater")
t_test_two_sample <- t.test(call_parent ~ condition, data = cox, var.equal = TRUE)
t_test_welch      <- t.test(call_parent ~ condition, data = cox, var.equal = FALSE)
t_test_paired     <- t.test(cox$affect_positive, cox$affect_negative, paired = TRUE)

t_test_one_sample
t_test_two_sample
t_test_welch
t_test_paired

# Tidy results
tidy_stats(t_test_one_sample)
tidy_stats(t_test_two_sample)
tidy_stats(t_test_welch)
tidy_stats(t_test_paired)

# Add stats
results <- results %>%
  add_stats(t_test_one_sample, identifier = "t_test_one_sample", type = "h",
            confirmatory = TRUE) %>%
  add_stats(t_test_two_sample) %>%
  add_stats(t_test_welch) %>%
  add_stats(t_test_paired)

# Analysis: cor.test() ------------------------------------------------------------------------

# Run correlations
correlation_pearson  <- cor.test(cox$call_parent, cox$anxiety, method = "pearson")
correlation_kendall  <- cor.test(cox$call_parent, cox$anxiety, method = "kendall")
correlation_spearman <- cor.test(cox$call_parent, cox$anxiety, method = "spearman")

# Tidy results
tidy_stats(correlation_pearson)
tidy_stats(correlation_kendall)
tidy_stats(correlation_spearman)

# Add stats
results <- results %>%
  add_stats(correlation_pearson) %>%
  add_stats(correlation_kendall) %>%
  add_stats(correlation_spearman)

# Analysis: chisq.test() ----------------------------------------------------------------------

# Get data
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent", "Republican"))

x <- c(A = 20, B = 15, C = 25)

# Run chi-squares
chi_square <- chisq.test(M)
chi_square_yates <- chisq.test(cox$condition, cox$sex)
chi_square_prob <- chisq.test(x)

# Tidy results
tidy_stats(chi_square)
tidy_stats(chi_square_yates)
tidy_stats(chi_square_prob)

# Add stats
results <- results %>%
  add_stats(chi_square) %>%
  add_stats(chi_square_yates) %>%
  add_stats(chi_square_prob)

# Analysis: wilcox.test() ---------------------------------------------------------------------

# Signed rank
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox_signed_rank <- wilcox.test(x, y, paired = TRUE, alternative = "greater")

wilcox_signed_rank_continuity_correction <- wilcox.test(Ozone ~ Month, data = airquality,
                                             subset = Month %in% c(5, 8))
# Rank sum
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
wilcox_rank_sum <- wilcox.test(x, y, alternative = "greater", exact = FALSE, correct = FALSE)

wilcox_rank_sum_conf <- wilcox.test(rnorm(10), rnorm(10, 2), conf.int = TRUE, conf.level = .9)

# Tidy results
tidy_stats(wilcox_signed_rank)
tidy_stats(wilcox_signed_rank_continuity_correction)
tidy_stats(wilcox_rank_sum)
tidy_stats(wilcox_rank_sum_conf)

# Add stats
results <- results %>%
  add_stats(wilcox_signed_rank) %>%
  add_stats(wilcox_signed_rank_continuity_correction) %>%
  add_stats(wilcox_rank_sum) %>%
  add_stats(wilcox_rank_sum_conf)

# Analysis: aov() -----------------------------------------------------------------------------

# Convert condition in the cox data frame to a factor
cox <- mutate(cox, condition = factor(condition))

# Prepare data for repeated measures ANOVAs
cox_long <- cox %>%
  select(ID, condition, anxiety, affect_positive, affect_negative) %>%
  gather("affect", "score", affect_positive, affect_negative) %>%
  arrange(ID) %>%
  mutate(
    ID = factor(ID),
    affect = factor(affect)
  )

# Run ANOVAs
aov_parent_condition <- aov(call_parent ~ condition, data = cox) # One-way ANOVA
aov_parent_condition_sex <- aov(call_parent ~ condition + sex, data = cox) # Two-way ANOVA
aov_parent_condition_x_sex <- aov(call_parent ~ condition * sex,
                                  data = cox) # Two-way ANOVA with interaction
aov_parent_condition_affect_negative <- aov(call_parent ~ condition + affect_negative,
                                            data = cox) # ANCOVA
aov_parent_affect <- aov(score ~ affect + Error(ID/affect),
                         data = cox_long) # One-within subject factor
aov_parent_condition_affect <- aov(score ~ condition * affect + Error(ID/affect) + condition,
                                   data = cox_long) # Mixed ANOVA
aov_parent_affect_anxiety <- aov(score ~ affect + anxiety + Error(ID/affect) + anxiety,
                                 data = cox_long) # ANCOVA with within subject factor

summary(aov_parent_condition)
summary(aov_parent_condition_sex)
summary(aov_parent_condition_x_sex)
summary(aov_parent_condition_affect_negative)
summary(aov_parent_affect)
summary(aov_parent_condition_affect)
summary(aov_parent_affect_anxiety)

# Tidy results
# TODO: Fix method
tidy_stats(aov_parent_condition)
tidy_stats(aov_parent_condition_sex)
tidy_stats(aov_parent_condition_x_sex)
tidy_stats(aov_parent_condition_affect_negative)
tidy_stats(aov_parent_affect)
tidy_stats(aov_parent_condition_affect)
tidy_stats(aov_parent_affect_anxiety)

# Add stats
results <- results %>%
  add_stats(aov_parent_condition) %>%
  add_stats(aov_parent_condition_sex) %>%
  add_stats(aov_parent_condition_x_sex) %>%
  add_stats(aov_parent_condition_affect_negative) %>%
  add_stats(aov_parent_affect) %>%
  add_stats(aov_parent_condition_affect) %>%
  add_stats(aov_parent_affect_anxiety)

# Analysis: lm() ------------------------------------------------------------------------------

# Run regressions
lm_parent_condition <- lm(call_parent ~ condition, data = cox)
lm_parent_condition_anxiety <- lm(call_parent ~ condition + anxiety, data = cox)
lm_parent_condition_x_anxiety <- lm(call_parent ~ condition * anxiety, data = cox)

summary(lm_parent_condition)
summary(lm_parent_condition_anxiety)
summary(lm_parent_condition_x_anxiety)

# Tidy results
tidy_stats(lm_parent_condition)
tidy_stats(lm_parent_condition_anxiety)
tidy_stats(lm_parent_condition_x_anxiety)

# Add stats
results <- results %>%
  add_stats(lm_parent_condition) %>%
  add_stats(lm_parent_condition_anxiety) %>%
  add_stats(lm_parent_condition_x_anxiety)

# Analysis: confint() -------------------------------------------------------------------------

# Run analysis
confint_lm <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint <- confint(confint_lm, level = .95)

# Add stats
results <- add_stats(results, confint, class = "confint")

# Add stats to model
results <- results %>%
  add_stats(confint_lm) %>%
  add_stats_to_model(confint, identifier = "confint_lm", class = "confint")

# Analysis: lme4’s lmer() ---------------------------------------------------------------------

# Load the package
library(lme4)

# Run multilevel models
lme4_1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lme4_2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)

data(Orthodont,package="nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
Orthodont$nsexage <- with(Orthodont, nsex*age)
lme4_3 <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) + (0 + nsexage|Subject),
               data = Orthodont)

summary(lme4_1)
summary(lme4_2)
summary(lme4_3)

# Tidy results
tidy_stats(lme4_1)
tidy_stats(lme4_2)
tidy_stats(lme4_3)

# Add stats
results <- results %>%
  add_stats(lme4_1) %>%
  add_stats(lme4_2) %>%
  add_stats(lme4_3)

# Analysis: lmerTest’s lmer() -----------------------------------------------------------------

# Load packages
library(lme4)
library(lmerTest)

# Run multilevel models
lmerTest4_1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lmerTest4_2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)

data(Orthodont,package="nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
Orthodont$nsexage <- with(Orthodont, nsex*age)
lmerTest4_3 <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) + (0 + nsexage|Subject),
                    data = Orthodont)

summary(lmerTest4_1)
summary(lmerTest4_2)
summary(lmerTest4_3)

# Tidy results
tidy_stats(lmerTest4_1)
tidy_stats(lmerTest4_2)
tidy_stats(lmerTest4_3)

# Add stats
results <- results %>%
  add_stats(lmerTest4_1) %>%
  add_stats(lmerTest4_2) %>%
  add_stats(lmerTest4_3)

# Analysis: psych’s alpha() -------------------------------------------------------------------

# Load package
library(psych)

# Run alpha
epi_extraversion_alpha <- alpha(select(epi, c("V1", "V3", "V8", "V10", "V13", "V17", "V22", "V25",
                                              "V27", "V39", "V44", "V46", "V49", "V53", "V56")))

# Tidy stats
tidy_stats(epi_extraversion_alpha)

# Add stats
results <- add_stats(results, epi_extraversion_alpha)

# Analysis: metafor ---------------------------------------------------------------------------

# Load package
library(metafor)

# Get data
dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

# Run meta-analysis
meta_analysis <- rma(yi, vi, data = dat, method = "REML", level = 90)
meta_analysis_mods <- rma(yi, vi, mods = cbind(ablat, year), data = dat, method = "REML")

meta_analysis
meta_analysis_mods

# Tidy results
tidy_stats(meta_analysis)
tidy_stats(meta_analysis_mods)

# Add stats
results <- results %>%
  add_stats(meta_analysis) %>%
  add_stats(meta_analysis_mods)

# Report results
report("meta_analysis", term = "intrcpt", results = results)
report("meta_analysis", term_nr = 1, statistic = "tau^2", results = results)
report("meta_analysis_mods", term = "ablat", results = results)

# Marino github issue example
library(metafor)
dat.bcg

res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",
           slab=paste(author, year, sep=", "), method="REML")
tidy_stats(res)
results <- list()
results <- add_stats(results, res, identifier = "marino_meta_analysis")
options(tidystats_list = results)
report("marino_meta_analysis", term = "intrcpt")
report("marino_meta_analysis", term = "(Heterogeneity)", s = "tau^2")
report("marino_meta_analysis", term_nr = 1, s = "tau^2")

# Analysis: ppcor’s pcor.test() ---------------------------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7,15,19,15,21,22,57,15,20,18),
  disp = c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
  deg = c(9,2,3,4,1,3,1,3,6,1),
  BC = c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00,4.48e-03,2.10e-06,0.00e+00)
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[,c("deg","BC")])
pcor_correlation

class(pcor_correlation)

# add_stats.data.frame() ----------------------------------------------------------------------

# Create a tidy data frame
x_squared_data <- data_frame(
  statistic = c("X-squared", "df", "p"),
  value = c(5.4885, 6, 0.4828),
  method = "Chi-squared test of independence"
)

# Add stats
results <- add_stats(results, x_squared_data, identifier = "x_squared")

# Create another tidy data frame
some_data <- tibble(
  term = c("group1", "group1", "group2", "group2"),
  statistic = c("t", "p", "t", "p"),
  value = c(5.4885, 0.04, 4.828, 0.06),
  method = "A test"
)

results <- add_stats(results, some_data, identifier = "some_data")

# add_stats(): default identifier -------------------------------------------------------------

# Statistical test
add_stats(list(), t_test_one_sample)

# Statistical test with piping
t.test(cox$call_parent, alternative = "greater") %>%
  add_stats(list(), .)

# Data frame
cox_avoidance <- cox %>%
  describe_data(avoidance) %>%
  tidy_describe_data()

add_stats(list(), cox_avoidance)

# Data frame with piping
cox %>%
  describe_data(avoidance) %>%
  tidy_describe_data() %>%
  add_stats(list(), ., type = "d")

# stats_list_to_df() --------------------------------------------------------------------------

results_data <- stats_list_to_df(results)
View(results_data)

# write_stats() -------------------------------------------------------------------------------

write_stats(results, path = "docs/tests/results.csv")

# In progress ---------------------------------------------------------------------------------

# rcorr()
library(Hmisc)

# Create some data
x <- c(-2, -1, 0, 1, 2)
y <- c(4, 1, 0, 1, 4)
z <- c(1, 2, 3, 4, NA)
v <- c(1, 2, 3, 4, 5)

# Perform analysis
rcorr_correlations <- rcorr(cbind(x,y,z,v))
rcorr_correlations

rcorr_correlations$r[upper.tri(rcorr_correlations$r, diag = TRUE)] <- NA
rcorr_correlations$n[upper.tri(rcorr_correlations$n, diag = TRUE)] <- NA
rcorr_correlations$P[upper.tri(rcorr_correlations$P, diag = TRUE)] <- NA

r <- as.data.frame(rcorr_correlations$r)
n <- as.data.frame(rcorr_correlations$n)
p <- as.data.frame(rcorr_correlations$P)

r <- rownames_to_column(r, "term")
n <- rownames_to_column(n, "term")
p <- rownames_to_column(p, "term")

r$statistic <- "r"
n$statistic <- "n"
p$statistic <- "p"

output <- bind_rows(r, n) %>%
  bind_rows(p) %>%
  gather("var", "value", -term, -term_nr, -statistic) %>%
  filter(!is.na(value)) %>%
  unite(term, var, term, sep = "-") %>%
  mutate(
    term_nr = 1:nrow(.),
    method = "rcorr() correlation {Hmisc}"
    ) %>%
  select(term, term_nr, statistic, value, method)

output <- output %>%
  separate(term, into = c("var", "columns"), sep = "-") %>%
  filter(statistic == "r") %>%
  mutate(term_nr = floor(term_nr / 4)) %>%
  spread(columns, value)

if (TRUE) {
  output <- arrange(output, desc(term_nr))
}

output <- select(output, -term_nr, -statistic, -method)
output <- select(output, c("var", names(sort(colSums(is.na(select(output, -var))), decreasing = T))))

# Analysis: anova()
anova(model3_1)

anova(model3_1, model3_2)

anova(model5_1)

anova(model5_1, model5_2)

# Logistic regression
data <- data_frame(
  DV = sample(c(0, 1), 20, replace = TRUE),
  IV1 = rnorm(20),
  IV2 = rnorm(20)
)

model6_1 <- glm(DV ~ IV1, data = data, family = binomial)

# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species*Petal.Width , data = iris), test = "Roy")

# tidyversity
# Install and load tidyversity
# install_github("mkearney/tidyversity")
library(tidyversity)

# Ordinary Least Squares (OLS)
TVM_1 <- tidy_regression(polcom, follow_trump ~ news_1 + ambiv_sexism_1)
tidy_summary(TVM_1)

# Logistic (dichotomous)
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1, type = "logistic") %>%
  tidy_summary()

# Poisson (count)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "poisson") %>%
  tidy_summary()

# Negative binomial (overdispersed)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()

# Robust and quasi- models
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "quasipoisson", robust = TRUE) %>%
  tidy_summary()

# ANOVA
polcom %>%
  dplyr::mutate(sex = ifelse(sex == 1, "Male", "Female"),
                vote_choice = dplyr::case_when(
                  vote_2016_choice == 1 ~ "Clinton",
                  vote_2016_choice == 2 ~ "Trump",
                  TRUE ~ "Other")) %>%
  tidy_anova(pp_party ~ sex * vote_choice) %>%
  tidy_summary()

# t-tests
polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()

# Structural equation modeling (SEM)
polcom %>%
  dplyr::mutate(therm_2 = 10 - therm_2 / 10,
                therm_1 = therm_1 / 10) %>%
  tidy_sem(news =~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
           ambiv_sexism =~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
             ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
           partisan =~ a*therm_1 + a*therm_2,
           ambiv_sexism ~ age + hhinc + edu + news + partisan) %>%
  tidy_summary()

# Cronbach's alpha
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)


# Inspect model -------------------------------------------------------------------------------

inspect_model(results, t_test_two_sample)
