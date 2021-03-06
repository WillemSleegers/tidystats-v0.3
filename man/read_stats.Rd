% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_stats.R
\name{read_stats}
\alias{read_stats}
\title{Read a .csv file that was produced with \code{write_stats}}
\usage{
read_stats(file)
}
\arguments{
\item{file}{Path to the tidy stats data file}
}
\description{
\code{read_stats} can convert a .csv file containing the statistical results that was produced with \code{write_stats}. It returns a list containing the results, with the identifier as the name for each list element.
}
\examples{
results <- read_stats(system.file("results.csv", package = "tidystats"))

}
