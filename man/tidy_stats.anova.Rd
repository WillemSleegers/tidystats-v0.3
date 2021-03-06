% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_stats.anova.r
\name{tidy_stats.anova}
\alias{tidy_stats.anova}
\title{Create a tidy stats data frame from an anova object}
\usage{
\method{tidy_stats}{anova}(model, args = NULL)
}
\arguments{
\item{model}{Output of \code{anova()}.}

\item{args}{Unused.}
}
\description{
\code{tidy_stats.anova} takes an anova object and tidies the output into a
data frame.
}
\examples{
# Regression example
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)

model_0 <- lm(weight ~ 1)
model_1 <- lm(weight ~ group)

tidy_stats(anova(model_0))
tidy_stats(anova(model_1))
tidy_stats(anova(model_0, model_1))
tidy_stats(anova(model_0, model_1, test = "Chisq"))

# Logistic regression example
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)

glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

tidy_stats(anova(glm.D93))

}
