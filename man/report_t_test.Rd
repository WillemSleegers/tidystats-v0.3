% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_t_test.R
\name{report_t_test}
\alias{report_t_test}
\title{Report function for t-tests}
\usage{
report_t_test(identifier, results = getOption("tidystats_list"))
}
\arguments{
\item{identifier}{A character string identifying the model.}

\item{results}{A tidystats list.}
}
\description{
Function to report a t-test in APA style.
}
\examples{
# Read in a list of results
results <- read_stats(system.file("results.csv", package = "tidystats"))

# Set the default tidystats list in options()
options(tidystats_list = results)

# Report results
report(identifier = "t_test_one_sample")
report(identifier = "t_test_two_sample")
report(identifier = "t_test_welch")
report(identifier = "t_test_paired")

}
