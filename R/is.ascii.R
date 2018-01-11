#' Identify non-ASCII elements in a character vector.
#'
#' Given a character vector, returns a logical vector of the same length
#' that identifies which elements contain non-ASCII (>127) characters.
#'
#' @param str A character vector.
#' @examples
#' print(str <- c('cat', 'caf\u00e9'))
#' is.ascii(str)
#' @export
is.ascii = function(str) {
  suppressWarnings(sapply(lapply(str, function(x) as.integer(charToRaw(x))), max) < 128)
}
