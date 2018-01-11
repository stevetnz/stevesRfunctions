# --------------------------------------------------------------------------------
#' Sorted unique values
#'
#' Returns the set of unique values, sorted.
#' @param x a vector.
#' @param incomparables as for \code{\link{unique}}.
#' @param decreasing,na.last as for \code{\link{sort}}.
#' @examples
#' uniques(c(1, 1, NA, 1, -1))
#' @export
uniques = function(x, incomparables=FALSE, decreasing=FALSE, na.last=TRUE)
  sort(unique(x, incomparables=incomparables), decreasing=decreasing, na.last=na.last)


# --------------------------------------------------------------------------------
#' Number of unique values
#'
#' Number of unique values, including NA as one of the values,
#' like \code{\link{unique}} does.
#' @param x a numeric vector
#' @param ... further parameters for \code{unique()}.
#' @examples
#' nuniques(letters)
#' @export
nuniques = function(x, ...) length(unique(x, ...))


# --------------------------------------------------------------------------------
#' Test whether a vector is sorted
#'
#' @param x a vector.
#' @param ... further parameters for \code{\link{is.unsorted}}.
#' @examples
#' is.sorted(LETTERS)
#' @export
is.sorted = function(x, ...) {
  !is.unsorted(x, ...)
}

# --------------------------------------------------------------------------------
#' Return values that are duplicates
#'
#' Given a vector, return the set of values that appear more than once.
#'
#' @param x a vector.
#' @param ... further parameters for \code{\link{duplicated}},
#' i.e. \code{incomparables=}.
#'
#' @examples
#' duplicates(c('cat','dog','dog'))
#' @export
duplicates = function(x, ...) uniques(x[duplicated(x, ...)])
