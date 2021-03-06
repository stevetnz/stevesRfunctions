% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grep-like_functions.R
\name{grepvi}
\alias{grepvi}
\alias{grepi}
\alias{grepv}
\title{Variations on grep with different defaults, for convenience.}
\usage{
grepvi(pattern, x, ...)

grepi(pattern, x, ...)

grepv(pattern, x, ...)
}
\arguments{
\item{pattern}{character string containing a regular expression (see \code{\link{grep}})}

\item{x}{a character vector where matches are sought (see \code{grep})}

\item{...}{other \code{grep()} parameters; see Details.}
}
\description{
Three convenient variations on \code{grep()} with modified defaults.  The suffix 'i' means ignore case and the suffix 'v' means return values.
}
\details{
These functions also accept other \code{grep} parameters (apart from \code{value=} and \code{ignore.case=}), specifically: \code{perl=}, \code{fixed=}, \code{useBytes=} and \code{invert=}.
}
\section{Functions}{
\itemize{
\item \code{grepvi}: return values, ignoring case.

\item \code{grepi}: the familiar grep but ignoring case.

\item \code{grepv}: case-sensitive grep returning values.
}}

\examples{
grepvi('[aeiou]', c(letters,LETTERS))
grepi('[aeiou]', c(letters,LETTERS))
grepv('[aeiou]', c(letters,LETTERS))
}
\seealso{
\link{\%like\%}
}
