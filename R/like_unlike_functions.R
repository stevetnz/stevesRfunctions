#' @rdname like
#' @name %like%
#' @title Binary operators for regexp string comparisons.
#' @description Inspired by the "like" operator in SQL,
#' these binary operators are a convenient way of using \code{\link{grepl}}.
#'
#' @seealso \link{grepvi}
#' @examples
#' lets = c(letters, LETTERS)
#' lets[lets %like% '[aeiou]']
#' lets[lets %Like% '[aeiou]']
#' lets[lets %unlike% '[aeiou]']
#' lets[lets %Unlike% '[aeiou]']
#' @describeIn like find matching values, ignoring case.
#' @export
`%like%` = function(x, y) grepl(y, x, ignore.case=TRUE)

#' @describeIn like case-sensitive matching.
#' @export
`%Like%` = function(x, y) grepl(y, x)

#' @describeIn like finds non-matching values, ignoring case.
#' @export
`%unlike%` = function(x, y) !grepl(y, x, ignore.case=TRUE)

#' @describeIn like finds non-matching values and is case-sensitive.
#' @export
`%Unlike%` = function(x, y) !grepl(y, x)
