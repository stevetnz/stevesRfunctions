#' Format numbers in constant width with leading zeros
#'
#' Formats numbers for alphabetic sorting by padding with zeros
#' to constant string width.
#' If width is not supplied, a default value is calculated
#' based on the values in \code{x}.
#'
#' @param x Vector of numbers.
#' @param width Width of character values returned.
#' @param digits Number of decimal places (defaults to 0).
#' @examples
#' paste0('ID', zeropad(1:20))
#' @export
zeropad = function(x, digits=0, width=NULL) {
  if (is.null(width))
    width = floor(max(log10(x))) + ifelse(digits>0, 2+digits, 1)
  formatC(x, format="f", width=width, digits=digits, flag='0')
}
