
# Descriptives --------------------------------------------------------------------------------

# count_data() --------------------------------------------------------------------------------

# Total N
count_data(cox)

# 1 categorical variable
count_data(cox, condition)

# 2 categorical variables
count_data(cox, condition, sex)

# 1 categorical variable, 1 grouping variable
cox %>%
  group_by(sex) %>%
  count_data(condition)

# tidy_count_data() ---------------------------------------------------------------------------

# Total N
count_data(cox) %>%
  tidy_count_data()

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

# 0 variables
describe_data(cox)

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
  add_stats(results, identifier = "D1_cox_condition", type = "d")

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


