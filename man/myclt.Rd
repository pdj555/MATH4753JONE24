% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myclt.R
\name{myclt}
\alias{myclt}
\title{Central Limit Theorem Demonstration Function}
\usage{
myclt(n, iter, a, b)
}
\arguments{
\item{n}{The number of samples to take.}

\item{iter}{The number of iterations to perform, each resulting in one sample mean.}

\item{a}{The minimum value of the uniform distribution (inclusive).}

\item{b}{The maximum value of the uniform distribution (exclusive).}
}
\value{
A vector of sample means.
}
\description{
This function takes samples from a uniform distribution and calculates their means to demonstrate the Central Limit Theorem.
}
\examples{
myclt(n = 10, iter = 1000, a = 0, b = 5)
}
