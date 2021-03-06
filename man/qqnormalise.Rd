% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numerics.R
\name{qqnormalise}
\alias{qqnormalise}
\title{Transform non-normal data using theoretical normal quantiles}
\usage{
qqnormalise(v, standardise = TRUE)
}
\arguments{
\item{v}{a numeric vector.}

\item{standardise}{logical; should the result be standardised?  Default TRUE.}
}
\description{
Note: if there are duplicate values, they will be randomly spread out to unique values; see the examples.
}
\examples{
layout(rbind(1:2))
x = round(rexp(50),1); plot(x, qqnormalise(x))
qqnorm(qqnormalise(x))
}
\seealso{
\link{qqnorm}
}
