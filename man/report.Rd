% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{report}
\alias{report}
\title{Report function}
\usage{
report(identifier, term = NULL, term_nr = NULL, var = NULL,
  group = NULL, statistic = NULL,
  results = getOption("tidystats_list"))
}
\arguments{
\item{identifier}{A character string identifying the model.}

\item{term}{A character string indicating which term you want to report the statistics of.}

\item{term_nr}{A number indicating which term you want to report the the statistics of.}

\item{var}{A character string identifying the variable.}

\item{group}{A character string identifying the group.}

\item{statistic}{A character string of a statistic you want to extract from a model.}

\item{results}{A tidy stats list.}
}
\description{
\code{report} is a general function that returns Markdown code of a statistical test in 6th edition APA style.
}
\details{
\code{report} calls a specific report function dependent on the type of statistical test that is supplied. The 'method' column of the statistical test is used to determine which report function to run.
}
\examples{
# Read in a list of results
results <- read_stats(system.file("results.csv", package = "tidystats"))

# Set the list as the default list
options(tidystats_list = results)

# Example: t-test
report("t_test_one_sample")
report("t_test_welch")

# Example: correlation
report("correlation_pearson")
report("correlation_spearman")

# Example: ANOVA
report("aov_two_way", term = "condition")
report("aov_two_way", term = "sex")

# Example: Linear models
report("lm_simple", term = "conditionmortality salience")
report("lm_simple", term_nr = 2)
report("lm_simple", group = "model")

}
