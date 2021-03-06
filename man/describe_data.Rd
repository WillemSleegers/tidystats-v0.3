% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe_data.r
\name{describe_data}
\alias{describe_data}
\title{Calculate common descriptive statistics}
\usage{
describe_data(data, ..., na.rm = TRUE)
}
\arguments{
\item{data}{A data frame.}

\item{...}{One or more unquoted (numerical) column names from the data frame,
separated by commas.}

\item{na.rm}{Logical. Should missing values (including NaN) be removed?}
}
\description{
\code{describe_data} returns a set of common descriptive statistics
(e.g., n, mean, sd) for numeric variables.
}
\details{
The data set can be grouped using \strong{dplyr}'s \code{group_by}
so that descriptives will be calculated for each group level.

Skew and kurtosis are based on the \code{skewness} and \code{kurtosis}
functions of the \strong{moments} package (Komsta & Novomestky, 2015).
}
\examples{
library(dplyr)

# 1 variable
describe_data(cox, avoidance)

# 1 variable, 1 group
cox \%>\%
  group_by(condition) \%>\%
  describe_data(avoidance)

# 2 variables
describe_data(cox, avoidance, anxiety)

# 2 variables, 1 group
cox \%>\%
  group_by(condition) \%>\%
  describe_data(avoidance, anxiety)

# 1 variable, 2 groups
cox \%>\%
  group_by(condition, sex) \%>\%
  describe_data(avoidance)

# 2 variables, 2 groups
cox \%>\%
  group_by(condition, sex) \%>\%
  describe_data(avoidance, anxiety)

}
