% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_stats.lmerMod.r
\name{tidy_stats.lmerMod}
\alias{tidy_stats.lmerMod}
\title{Create a tidy stats data frame from an lmerMod object}
\usage{
\method{tidy_stats}{lmerMod}(model, args = NULL)
}
\arguments{
\item{model}{Output of lme4's \code{lmer()}.}

\item{args}{Unused.}
}
\description{
\code{tidy_stats.lmerMod} takes an lmerMod object and converts the object to
a tidy stats data frame.
}
\examples{
# Check if lme4 package is available
if(!requireNamespace("lme4", quietly = TRUE)) {

  message(paste0("Package 'lme4' is needed for this example to work. ",
              "Please install it."), .call = FALSE) 
} else {

  # Conduct a linear mixed model
  model_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)

  # Tidy stats
  tidy_stats(model_lmer)
}

}
