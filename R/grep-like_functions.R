
#' @export
grepi = function(...) grep(..., ignore.case=TRUE)

#' @export
grepv = function(...) grep(..., value=TRUE)

#' @export
grepvi = function(...) grep(..., value=TRUE, ignore.case=TRUE)

#' @export
`%like%` = function(x, y) grepl(y, x, ignore.case=TRUE)

#' @export
`%Like%` = function(x, y) grepl(y, x)

#' @export
`%unlike%` = function(x, y) !grepl(y, x, ignore.case=TRUE)

#' @export
`%Unlike%` = function(x, y) !grepl(y, x)

