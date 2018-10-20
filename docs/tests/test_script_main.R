
# Setup -------------------------------------------------------------------

# Load packages
library(devtools)
document()
install()

# Set options
options(pillar.sigfig = 5)

# install_github("willemsleegers/tidystats")
library(tidystats)
library(tidyverse)

# Create empty tidy stats data frame
results <- list()

# Analysis: t.test() ------------------------------------------------------

# Run t-tests
t_test_one_sample <- t.test(cox$call_parent, alternative = "greater")
t_test_two_sample <- t.test(call_parent ~ condition, data = cox,
  var.equal = TRUE)
t_test_welch <- t.test(call_parent ~ condition, data = cox,
  var.equal = FALSE)
t_test_paired <- t.test(cox$affect_positive, cox$affect_negative,
  paired = TRUE)

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
  add_stats(t_test_one_sample, identifier = "t_test_one_sample") %>%
  add_stats(t_test_two_sample) %>%
  add_stats(t_test_welch) %>%
  add_stats(t_test_paired)

# Analysis: cor.test() ----------------------------------------------------

# Run correlations
correlation_pearson <- cor.test(cox$call_parent, cox$anxiety,
  method = "pearson")
correlation_kendall <- cor.test(cox$call_parent, cox$anxiety,
  method = "kendall")
correlation_spearman <- cor.test(cox$call_parent, cox$anxiety,
  method = "spearman")

correlation_pearson
correlation_kendall
correlation_spearman

# Tidy results
tidy_stats(correlation_pearson)
tidy_stats(correlation_kendall)
tidy_stats(correlation_spearman)

# Add stats
results <- results %>%
  add_stats(correlation_pearson) %>%
  add_stats(correlation_kendall) %>%
  add_stats(correlation_spearman)

# Analysis: chisq.test() --------------------------------------------------

# Get data
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent",
  "Republican"))
x <- c(A = 20, B = 15, C = 25)

# Run chi-squares
chi_squared <- chisq.test(M)
chi_squared_yates <- chisq.test(cox$condition, cox$sex)
chi_squared_prob <- chisq.test(x)

chi_squared
chi_squared_yates
chi_squared_prob

# Tidy results
tidy_stats(chi_squared)
tidy_stats(chi_squared_yates)
tidy_stats(chi_squared_prob)

# Add stats
results <- results %>%
  add_stats(chi_squared) %>%
  add_stats(chi_squared_yates) %>%
  add_stats(chi_squared_prob)

# Analysis: wilcox.test() -------------------------------------------------

# Signed rank
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcoxon_signed_rank <- wilcox.test(x, y, paired = TRUE,
  alternative = "greater")

wilcoxon_rank_sum_continuity <- wilcox.test(Ozone ~ Month, data = airquality,
  subset = Month %in% c(5, 8))

wilcoxon_signed_rank
wilcoxon_rank_sum_continuity

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)

wilcoxon_rank_sum <- wilcox.test(x, y, alternative = "greater", exact = FALSE,
  correct = FALSE)
wilcoxon_rank_sum_conf <- wilcox.test(x, y, conf.int = TRUE, conf.level = .9)

wilcoxon_rank_sum
wilcoxon_rank_sum_conf

# Tidy results
tidy_stats(wilcoxon_signed_rank)
tidy_stats(wilcoxon_rank_sum_continuity)
tidy_stats(wilcoxon_rank_sum)
tidy_stats(wilcoxon_rank_sum_conf)

# Add stats
results <- results %>%
  add_stats(wilcoxon_signed_rank) %>%
  add_stats(wilcoxon_rank_sum_continuity) %>%
  add_stats(wilcoxon_rank_sum) %>%
  add_stats(wilcoxon_rank_sum_conf)

# Analysis: Fisher’s test -------------------------------------------------

# Get data
TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2,
  dimnames = list(Guess = c("Milk", "Tea"), Truth = c("Milk", "Tea")))

Convictions <- matrix(c(2, 10, 15, 3), nrow = 2, dimnames =
    list(c("Dizygotic", "Monozygotic"), c("Convicted", "Not convicted")))

Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
  dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
    satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))

MP6 <- rbind(
  c(1,2,2,1,1,0,1),
  c(2,0,0,2,3,0,0),
  c(0,1,1,1,2,7,3),
  c(1,1,2,0,0,0,1),
  c(0,1,1,1,1,0,0)
  )

# Run Fisher tests
fisher_test <- fisher.test(TeaTasting)
fisher_test_alternative_greater <- fisher.test(TeaTasting,
  alternative = "greater")
fisher_test_alternative_less <- fisher.test(Convictions, alternative = "less")
fisher_test_no_CI <- fisher.test(Convictions, conf.int = FALSE)
fisher_test_r_by_c <- fisher.test(Job)
fisher_test_simulated_p <- fisher.test(Job, simulate.p.value = TRUE, B = 1e5)
fisher_test_hybrid <- fisher.test(MP6, hybrid = TRUE)

fisher_test
fisher_test_alternative_greater
fisher_test_alternative_less
fisher_test_no_CI
fisher_test_r_by_c
fisher_test_simulated_p
fisher_test_hybrid

# Tidy stats
tidy_stats(fisher_test)
tidy_stats(fisher_test_alternative_greater)
tidy_stats(fisher_test_alternative_less)
tidy_stats(fisher_test_no_CI)
tidy_stats(fisher_test_r_by_c)
tidy_stats(fisher_test_simulated_p)
tidy_stats(fisher_test_hybrid)

# Add stats
results <- results %>%
  add_stats(fisher_test) %>%
  add_stats(fisher_test_alternative_greater) %>%
  add_stats(fisher_test_alternative_less) %>%
  add_stats(fisher_test_no_CI) %>%
  add_stats(fisher_test_r_by_c) %>%
  add_stats(fisher_test_simulated_p) %>%
  add_stats(fisher_test_hybrid)

# Analysis: aov() ---------------------------------------------------------

# Convert condition and sex in the cox data frame to a factor
cox <- mutate(cox,
  condition = factor(condition),
  sex = factor(sex)
  )

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
# One-way ANOVA
aov_one_way <- aov(call_parent ~ condition, data = cox)
# Two-way ANOVA
aov_two_way <- aov(call_parent ~ condition + sex, data = cox)
# Two-way ANOVA with interaction
aov_two_way_interaction <- aov(call_parent ~ condition * sex, data = cox)
# ANCOVA
aov_ancova <- aov(call_parent ~ condition + affect_negative, data = cox)
# One-within subject factor
aov_one_within <- aov(score ~ affect + Error(ID/affect), data = cox_long)
# Mixed ANOVA
aov_mixed <- aov(score ~ condition * affect + Error(ID/affect) + condition,
  data = cox_long)
# ANCOVA with within subject factor
aov_ancova_with_within <- aov(score ~ affect + anxiety + Error(ID/affect) +
    anxiety, data = cox_long)

summary(aov_one_way)
summary(aov_two_way)
summary(aov_two_way_interaction)
summary(aov_ancova)
summary(aov_one_within)
summary(aov_mixed)
summary(aov_ancova_with_within)

# Tidy results
tidy_stats(aov_one_way)
tidy_stats(aov_two_way)
tidy_stats(aov_two_way_interaction)
tidy_stats(aov_ancova)
tidy_stats(aov_one_within)
tidy_stats(aov_mixed)
tidy_stats(aov_ancova_with_within)

# Add stats
results <- results %>%
  add_stats(aov_one_way) %>%
  add_stats(aov_two_way) %>%
  add_stats(aov_two_way_interaction) %>%
  add_stats(aov_ancova) %>%
  add_stats(aov_one_within) %>%
  add_stats(aov_mixed) %>%
  add_stats(aov_ancova_with_within)

# Analysis: lm() ----------------------------------------------------------

# Run regressions
lm_simple <- lm(call_parent ~ condition, data = cox)
lm_multiple <- lm(call_parent ~ condition + anxiety, data = cox)
lm_interaction <- lm(call_parent ~ condition * anxiety, data = cox)

summary(lm_simple)
summary(lm_multiple)
summary(lm_interaction)

# Tidy results
tidy_stats(lm_simple)
tidy_stats(lm_multiple)
tidy_stats(lm_interaction)

# Add stats
results <- results %>%
  add_stats(lm_simple) %>%
  add_stats(lm_multiple) %>%
  add_stats(lm_interaction)

# Analysis: glm() ---------------------------------------------------------

# Example 1: Dobson (1990) Page 93: Randomized Controlled Trial
# Get data
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)

# Run model
glm_poisson <- glm(counts ~ outcome + treatment, family = poisson())
summary(glm_poisson)

# Tidy data
tidy_stats(glm_poisson)

# Example 2: Venables & Ripley (2002, p.189)
# Get data
utils::data(anorexia, package = "MASS")

# Run model
glm_gaussian <- glm(Postwt ~ Prewt + Treat + offset(Prewt), data = anorexia)
summary(glm_gaussian)

# Tidy data
tidy_stats(glm_gaussian)

# Example 3: McCullagh & Nelder (1989, pp. 300-2)
# Get data
clotting <- data_frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12)
)

# Run model
glm_gamma <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
summary(glm_gamma)

# Tidy stats
tidy_stats(glm_gamma)

# Add stats
results <- results %>%
  add_stats(glm_poisson) %>%
  add_stats(glm_gaussian) %>%
  add_stats(glm_gamma)

# Analysis: confint() -----------------------------------------------------

# Run analysis
lm_confint <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint_lm_95 <- confint(lm_confint, level = .95)
confint_lm_90 <- confint(lm_confint, level = .90)

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm_confint <- glm(counts ~ outcome + treatment, family = poisson())
confint_glm_profile_likelihood <- confint(glm_confint)
confint_glm_asymptotic_normality <- confint.default(glm_confint)

# Add stats
add_stats(list(), confint_lm_95, class = "confint")
add_stats(list(), confint_lm_90, class = "confint")
add_stats(list(), confint_glm_profile_likelihood, class = "confint")
add_stats(list(), confint_glm_asymptotic_normality, class = "confint")

# Add stats to model
results <- results %>%
  add_stats(lm_confint) %>%
  add_stats(confint_lm_95, class = "confint") %>%
  add_stats(confint_lm_90, class = "confint") %>%
  add_stats_to_model(confint_lm_95, identifier = "lm_confint",
    class = "confint") %>%
  add_stats(confint_glm_profile_likelihood, class = "confint") %>%
  add_stats(confint_glm_asymptotic_normality, class = "confint")

# Analysis: lme4’s lmer() -------------------------------------------------

# Load the package
library(lme4)

# Run multilevel models
lme4_lme <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lme4_lme_uncorrelated <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)

data(Orthodont, package = "nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
Orthodont$nsexage <- with(Orthodont, nsex * age)
lme4_lme_dummies <- lmer(distance ~ age + (age|Subject) + (0 + nsex|Subject) +
                 (0 + nsexage|Subject), data = Orthodont)

summary(lme4_lme)
summary(lme4_lme_uncorrelated)
summary(lme4_lme_dummies)

# Tidy results
tidy_stats(lme4_lme)
tidy_stats(lme4_lme_uncorrelated)
tidy_stats(lme4_lme_dummies)

# Add stats
results <- results %>%
  add_stats(lme4_lme) %>%
  add_stats(lme4_lme_uncorrelated) %>%
  add_stats(lme4_lme_dummies)

# Analysis: lmerTest’s lmer() ---------------------------------------------

# Load packages
library(lme4)
library(lmerTest)

# Run multilevel models
lmerTest_lme <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
lmerTest_lme_uncorrelated <- lmer(Reaction ~ Days + (Days || Subject),
  sleepstudy)

data(Orthodont,package="nlme")
Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
Orthodont$nsexage <- with(Orthodont, nsex*age)
lmerTest_lme_dummies <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
                      (0 + nsexage|Subject), data = Orthodont)

summary(lmerTest_lme)
summary(lmerTest_lme_uncorrelated)
summary(lmerTest_lme_dummies)

# Tidy results
tidy_stats(lmerTest_lme)
tidy_stats(lmerTest_lme_uncorrelated)
tidy_stats(lmerTest_lme_dummies)

# Add stats
results <- results %>%
  add_stats(lmerTest_lme) %>%
  add_stats(lmerTest_lme_uncorrelated) %>%
  add_stats(lmerTest_lme_dummies)

# Analysis: psych ---------------------------------------------------------

# Load package
library(psych)

# Analysis: psych’s alpha() -----------------------------------------------

# Run alpha
psych_alpha <- alpha(dplyr::select(epi, V1, V3, V8, V10, V13, V17,
  V22, V25, V27, V39, V44, V46, V49, V53, V56))

# Tidy stats
tidy_stats(psych_alpha)

# Add stats
results <- add_stats(results, psych_alpha)

# Analysis: psych’s corr.test() -------------------------------------------

# Get some data
attitude <- datasets::attitude

# Run analysis
psych_correlations <- corr.test(attitude, adjust = "none")
print(psych_correlations, short = FALSE)

# Tidy results
tidy_stats(psych_correlations)

# Add stats
results <- add_stats(results, psych_correlations)

# Analysis: psych's ICC ---------------------------------------------------

# Load data
sf <- matrix(ncol = 4, byrow = TRUE,
  c(9,  2, 5, 8,
    6,  1, 3, 2,
    8,  4, 6, 8,
    7,  1, 2, 6,
    10, 5, 6, 9,
    6,  2, 4, 7))
colnames(sf) <- paste("J", 1:4, sep = "")
rownames(sf) <- paste("S", 1:6, sep = "")

# Perform analysis
psych_ICC <- ICC(sf, lmer = FALSE)

# Tidy stats
tidy_stats(psych_ICC)

# Add stats
results <- add_stats(results, psych_ICC)

# add_stats.data.frame() --------------------------------------------------

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

# add_stats(): default identifier -----------------------------------------

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

# stats_list_to_df() ------------------------------------------------------

results_data <- stats_list_to_df(results)
View(results_data)

# write_stats() -----------------------------------------------------------

write_stats(results, path = "tests/testthat/test_results.csv")

# In progress -------------------------------------------------------------

# Analysis: metafor -------------------------------------------------------

# Load package
library(metafor)

# Get data
dat <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg)

# Run univariate meta-analyses
rma_uni <- rma(yi, vi, data = dat, method = "REML", level = 90)
rma_uni_mods <- rma(yi, vi, mods = cbind(ablat, year), data = dat,
  method = "REML")

rma_uni
rma_uni_mods

# Tidy results
tidy_stats(rma_uni)
tidy_stats(rma_uni_mods)

# Add stats
results <- results %>%
  add_stats(rma_uni) %>%
  add_stats(rma_uni_mods)

# Run multivariate meta-analyses

# Prepare data
# Change data into long format
dat.long <- to.long(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = dat.bcg)

# Set levels of group variable
levels(dat.long$group) <- c("exp", "con")

# Set "con" to reference level
dat.long$group <- relevel(dat.long$group, ref = "con")

# Calculate log odds and corresponding sampling variances
dat.long <- escalc(measure = "PLO", xi = out1, mi = out2, data = dat.long)
dat.long$effect <- 1:nrow(dat.long)

# Bivariate random-effects model using rma.mv()
rma_mv <- rma.mv(yi, vi, random = ~ group | study/effect, struct="UN",
  data = dat.long)
rma_mv_mods <- rma.mv(yi, vi, mods = ~ group, random = ~ group | study,
  struct="UN", data=dat.long)

rma_mv
rma_mv_mods

# Tidy stats
tidy_stats(rma_mv)
tidy_stats(rma_mv_mods)

# Add stats
results <- results %>%
  add_stats(rma_mv) %>%
  add_stats(rma_mv_mods)

# Inspect resulst
inspect(results)

# Report results

report_rma(results, "rma_uni", statistic = NULL)
report_rma(results, "rma_uni", term = "intrcpt")

report(results = results, identifier = "rma_uni", term = "intrcpt")

report_rma(results, "rma_mv", group = "heterogeneity")
report_rma(results, "rma_mv", term = "intrcpt")

report("rma_uni", term = "intrcpt", results = results)
report("meta_analysis", term_nr = 1, statistic = "tau^2", results = results)
report("meta_analysis_mods", term = "ablat", results = results)

# Marino github issue example

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
output <- select(output, c("var",
                           names(sort(colSums(is.na(select(output, -var))),
                                      decreasing = T))))

# Analysis: anova()
anova(model3_1)

anova(model3_1, model3_2)

anova(model5_1)

anova(model5_1, model5_2)


# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
                           data = iris), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
                           data = iris), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
                           data = iris), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species *
                             Petal.Width , data = iris), test = "Roy")

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
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "quasipoisson",
                  robust = TRUE) %>%
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

# Analysis: ppcor’s pcor.test() -------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7,15,19,15,21,22,57,15,20,18),
  disp = c(0.000,0.964,0.000,0.000,0.921,0.000,0.000,1.006,0.000,1.011),
  deg = c(9,2,3,4,1,3,1,3,6,1),
  BC = c(1.78e-02,1.05e-06,1.37e-05,7.18e-03,0.00e+00,0.00e+00,0.00e+00,
    4.48e-03,2.10e-06,0.00e+00)
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[,c("deg","BC")])
pcor_correlation



# Inspect model -----------------------------------------------------------

inspect(results)
inspect(t_test_one_sample)
