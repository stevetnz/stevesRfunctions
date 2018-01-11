#' Convert a logical vector to a factor
#'
#' Converts a logical vector to a factor conatining two levels.
#' This is useful in statistical models such as logistic regression,
#' both for the dependent variable and the explanatory variables.
#'
#' @param bool A logical vector, or a vector that can be coerced to logical.
#' @return A factor with levels combining the unique levels of the input factors.
#' @export
NoYes = function(bool, ifFALSE='No', ifTRUE='Yes') {
  if(!is.logical(bool)) bool = as.logical(bool)
  factor(ifelse(bool, ifTRUE, ifFALSE), levels=c(ifFALSE, ifTRUE))
}
