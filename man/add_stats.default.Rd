% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_stats.default.r
\name{add_stats.default}
\alias{add_stats.default}
\title{add_stats default function}
\usage{
\method{add_stats}{default}(results, output, identifier = NULL,
  type = NULL, confirmatory = NULL, notes = NULL, class = NULL,
  args = NULL)
}
\arguments{
\item{results}{A tidy stats list.}

\item{output}{Output of a statistical test.}

\item{identifier}{A character string identifying the model. Automatically
created if not provided.}

\item{type}{A character string indicating the type of test. One of
"hypothesis", "manipulation check", "contrast", "descriptives", or "other".
Can be abbreviated.}

\item{confirmatory}{A boolean to indicate whether the statistical test was
confirmatory (TRUE) or exploratory (FALSE). Can be NA.}

\item{notes}{A character string to add additional information. Some
statistical tests produce notes information, which will be overwritten if
notes are provided.}

\item{class}{A character string to indicate which function was used to
produce the output. See 'Details' for a list of supported functions.}

\item{args}{An optional list of additional arguments. Can be used to specify
how model results should be summarized.}
}
\description{
\code{add_stats.default} is the default add_stats function, which takes the
output of a statistical test, tidies the output, and adds it to a tidystats
list.
}
\examples{
# Create an empty list to store the results in
results <- list()

# Example: t-test
model_t_test <- t.test(extra ~ group, data = sleep)
results <- add_stats(results, model_t_test, identifier = "t_test")

# Example: correlation
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

model_correlation <- cor.test(x, y)

# Add output to the results list, only storing the correlation and p-value
results <- add_stats(results, model_correlation, identifier = "correlation")

# Example: Regression
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

model_lm <- lm(weight ~ group)

# Add output to the results list, with notes
results <- add_stats(results, model_lm, identifier = "regression", notes =
  "regression example")

# Example: ANOVA
model_aov <- aov(yield ~ block + N * P * K, npk)

results <- add_stats(results, model_aov, identifier = "ANOVA")

# Example: Within-Subjects ANOVA
model_aov_within <- aov(extra ~ group + Error(ID/group), data = sleep)

results <- add_stats(results, model_aov_within, identifier = "ANOVA_within")

}
