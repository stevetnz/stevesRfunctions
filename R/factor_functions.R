#' Convert a logical vector to a factor
#'
#' @usage NoYes(bool)
#' @param bool A logical vector, or a vector that can be coerced to logical.
#' @return A factor with levels combining the unique levels of the input factors.
#' @export
NoYes = function(bool) {
  if(!is.logical(bool)) bool = as.logical(bool)
  factor(ifelse(bool, "Yes", "No"), levels=c("No", "Yes"))
}
