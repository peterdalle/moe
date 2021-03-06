% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moe.r
\name{moe}
\alias{moe}
\title{Calculate margin of error for simple probability samples}
\usage{
moe(proportion, n, conf.level = 0.95, digits = 2,
  population.correction = FALSE, population.size = NULL)
}
\arguments{
\item{proportion}{value between 0 and 1 indicating the proportion, such as 0.30 for 30 percent.}

\item{n}{sample size.}

\item{conf.level}{confidence level (defaults to \code{0.95}).}

\item{digits}{number of decimal digits used when formatting the results as APA and human-readable messages (defaults to \code{2}).}

\item{population.correction}{whether or not results should be corrected by population size (defaults to \code{FALSE}), using the Finite Population Correction technique.}

\item{population.size}{population size used by the population correction (defaults to \code{NULL}). Only used if \code{population.correction} is set to \code{TRUE}.}
}
\value{
a list with \code{margin.of.error} (margin of error), \code{conf.level} (confidence level), \code{conf.lower} (confidence interval lower bound), \code{conf.upper} (confidence interval upper bound), \code{proportion} (proportion), \code{percentage} (percentage), \code{z.value} (z-value from normal distribution), \code{digits} (number of digits used to format APA confidence intervals), \code{n} (sample size), \code{population.corrected} (whether or not the margin of error is corrected for population size), \code{population.size} (population size), \code{fpc} (finite population correction, between 0 and 1), \code{sampling.fraction} (sampling fraction, ratio of sample size to population size, between 0 and 1), \code{error.uncorrected} (margin of error before it is corrected for population size), and \code{apa} (APA6 style formatted confidence intervals).
}
\description{
\code{moe} calculates margin of error and confidence intervals for simple probability
samples, as well as results formatted in accordance with American Psychological Association standards (APA6).
}
\section{Assumptions}{

\code{moe} assumes a normal distribution by calculating the z-value from \code{qnorm}, as well as simple random sampling (i.e., all observations have an equal probability of inclusion).
}

\section{Finite population correction}{

When the sampling fraction (ratio of sample size to population size) is large, approximately 5 percent or more, the estimate of the standard error can be corrected by multiplying a \href{https://en.wikipedia.org/wiki/Standard_error#Correction_for_finite_population}{Finite Population Correction}. To use this correction, set the \code{population.correction} argument to \code{TRUE} and set the sample size for the population using the \code{population.size} argument.
}

\section{References}{

Bondy, W. & Zlot, W. (1976). The Standard Error of the Mean and the Difference Between Means for Finite Populations. The American Statistician, 30, 96–97. doi:10.2307/2683803
}

\examples{
\dontrun{
# Margin of error for party with 30\%
# of voters in a sample of 1,200.
moe(proportion=0.30, n=1200)

# Correct for population size (N=300,000),
# using 99\% confidence level.
moe(proportion=0.30, n=1200, conf.level=0.99,
    population.correction=TRUE, population.size=300000)

# Get confidence interval.
m <- moe(proportion=0.30, n=1200)
m$conf.lower
m$conf.upper

# Show all information.
summary(m, digits=2)

# APA6 style confidence intervals.
as.character(m, digits=2)

# Print margin of error.
print(m, digits=2)

# 2-sample test for equality of proportions
# (Chi-square) using the minus operator.
m1 <- moe(proportion=0.33, n=1200)
m2 <- moe(proportion=0.40, n=1200)
m1 - m2
}
}
\seealso{
\link[stats]{qnorm()}
}
