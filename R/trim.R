#' Trim leading and trailing white space
#'
#' Removes white space from the start and end of each element
#' in a character vector (or other similar character object, e.g. matrix).
#'
#' @param txt Character object
#' @examples
#' trim(c('    abc','xyz  ','\t\tindented.  '))
#' @export
trim = function(txt) {
  gsub("^[ \t]+|[ \t]+$", "", txt)
}
