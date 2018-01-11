#' Variations on grep with different defaults, for convenience.
#'
#' Three convenient variations on \code{grep()} with modified defaults.  The suffix 'i' means ignore case and the suffix 'v' means return values.
#' @param pattern character string containing a regular expression (see \code{\link{grep}})
#' @param x a character vector where matches are sought (see \code{grep})
#' @param ... other \code{grep()} parameters; see Details.
#'
#' @details
#' These functions also accept other \code{grep} parameters (apart from \code{value=} and \code{ignore.case=}), specifically: \code{perl=}, \code{fixed=}, \code{useBytes=} and \code{invert=}.
#' @describeIn grepvi return values, ignoring case.
#' @examples
#' grepvi('[aeiou]', c(letters,LETTERS))
#' grepi('[aeiou]', c(letters,LETTERS))
#' grepv('[aeiou]', c(letters,LETTERS))
# can't seem to get this to work: @seealso \link{%like%}
#' @seealso \link{like}
#' @export
grepvi = function(pattern, x, ...)
  grep(pattern=pattern, x=x, ..., value=TRUE, ignore.case=TRUE)

#' @export
#' @describeIn grepvi the familiar grep but ignoring case.
grepi = function(pattern, x, ...)
  grep(pattern=pattern, x=x, ..., ignore.case=TRUE)

#' @export
#' @describeIn grepvi case-sensitive grep returning values.
grepv = function(pattern, x, ...)
  grep(pattern=pattern, x=x, ..., value=TRUE)
