% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_stats.htest.R
\name{tidy_stats.htest}
\alias{tidy_stats.htest}
\title{Create a tidy stats data frame from an htest object}
\usage{
\method{tidy_stats}{htest}(model, args = NULL)
}
\arguments{
\item{model}{Output of \code{t.test()}.}

\item{args}{Unused.}
}
\description{
\code{tidy_stats.htest} takes an htest object and converts the object to a tidy stats data frame.
}
\examples{
# Conduct a t-test
model_t_test <- t.test(extra ~ group, data = sleep)
tidy_stats(model_t_test)

# Conduct a correlation
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

model_correlation <- cor.test(x, y)
tidy_stats(model_correlation)

# Conduct a chi-square test
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))

model_chi_square <- chisq.test(M)
tidy_stats(model_chi_square)

}
