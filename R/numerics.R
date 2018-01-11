# --------------------------------------------------------------------------------
#' Fractional part of numeric object
#' @export
frac = function(x, trunc=FALSE) {
  if (trunc) x - trunc(x)
  else x - floor(x)
}

# --------------------------------------------------------------------------------
#' Skewness of a vector
#'
#' Return the skewness of the distribution for a numeric vector.
#'
#' @param x numeric vector
#' @param na.rm logical; should missing values be removed?
#' @examples
#' skewness(rnorm(1000))
#' skewness(rexp(1000))
#' @seealso \link{BoxCox}, \link{kurtosis}
#' @export
skewness = function(x, na.rm=TRUE) {
  if (na.rm) x = x[!is.na(x)]
  xdiffs = x - mean(x)
  mean(xdiffs ^ 3) / (mean(xdiffs ^ 2) ^ (3/2))
}

# --------------------------------------------------------------------------------
#' Kurtosis of a vector
#'
#' Computes the excess or absolute kurtosis of a vector.
#' By default, it returns the excess kurtosis, so that (for example)
#' the Normal distribution returns values near zero.
#'
#' @param x numeric vector
#' @param excess logical; compute excess rather than absolute kurtosis?
#' @param na.rm logical; should missing values be removed?
#' @examples
#' kurtosis(rnorm(1000))
#' kurtosis(rnorm(1000), excess=FALSE)
#' kurtosis(runif(1000))
#' kurtosis(rcauchy(1000))
#' @seealso \link{skewness}
#' @export
kurtosis = function(x, excess=TRUE, na.rm=TRUE) {
  n = length(x)
  if (na.rm) x = x[!is.na(x)]
  sq.diffs = (x - mean(x)) ^ 2
  n * sum(sq.diffs ^ 2) / (sum(sq.diffs) ^ 2) - 3 * excess
}

# --------------------------------------------------------------------------------
#' Box-Cox power transformation of a vector, adjusted to retain mean and SD.
#'
#' Perform a power transformation on a numeric vector, retaining (by default)
#' its mean and standard deviation.
#' If two lamda values are given, this range is searched to find a power
#' transformation parameter that minimises \code{\link{skewness}}.
#' If \code{x} contains non-positive values, the vector is first shifted
#' into the positive Reals before the transformation.
#'
#' @param x a numeric vector
#' @param lamda Either a single power parameter for the transformation
#' or a vector of length 2 giving the range of lamda values to search
#' to find an optimised transformation parameter.
#' @param standardise logical.  If TRUE, the returned value has mean=0 and SD=1.
#' Otherwise (the default) the original mean and SD are retained.
#' @examples
#' x = rexp(50); xnew = BoxCox(x)
#' plot(x, xnew, ylab=paste0('BoxCox(x) used lamda=',signif(attr(xnew,'lamda'),4)))
#' qqnorm(BoxCox(x))
#' @export
BoxCox = function(x, lamda=c(-5,5), standardise=FALSE) {
  m = mean(x, na.rm=TRUE)
  s = sd(x, na.rm=TRUE)
  if ((minx <- min(x, na.rm=TRUE)) <= 0)
    x = x + abs(minx) + 0.01 * diff(range(x, na.rm=TRUE))
  if (length(lamda)==2) {
    fn = function(l) skewness(BoxCox(x, l))^2
    lamda = optimise(fn, lamda)$minimum
  }
  z = if (lamda==0) log(x) else (x^lamda - 1) / lamda
  z[is.infinite(z)] = NA
  attr(z,'lamda') = lamda
  if (standardise)
    (z - mean(z, na.rm=TRUE)) / sd(z, na.rm=TRUE)
  else
    m + (z - mean(z, na.rm=TRUE)) * (s / sd(z, na.rm=TRUE))
}

# --------------------------------------------------------------------------------
#' Transform non-normal data using theoretical normal quantiles
#'
#' Note: if there are duplicate values, they will be randomly spread out to unique values; see the examples.
#' @param v a numeric vector.
#' @param standardise logical; should the result be standardised?  Default TRUE.
#' @examples
#' layout(rbind(1:2))
#' x = round(rexp(50),1); plot(x, qqnormalise(x))
#' qqnorm(qqnormalise(x))
#' @seealso \link{qqnorm}
#' @export
qqnormalise = function(v, standardise=TRUE) {
  res = qqnorm(v, plot.it=FALSE)$x
  if (standardise) return(res)
  res * (sd(v, na.rm=TRUE) / sd(res, na.rm=TRUE)) + mean(v, na.rm=TRUE)
}


# --------------------------------------------------------------------------------
#' Fuzzy (random) rounding.
#'
#' Values are rounded at random, with probabilities determined by
#' how close they are to the integers either side.
#' This may be useful for interesting colour dithering in images.
#'
#' @param x a numeric vector or matrix.
#' @examples
#' x = seq(0,20,length.out=200); plot(x, fuzzyround(x))
#' image(fuzzyround(toeplitz(seq(5,1,length.out=200))), col=heat.colours())
#' @export
fuzzyround = function(x) {
  ifelse(frac(x) > runif(length(x)), ceiling(x), floor(x))
}
