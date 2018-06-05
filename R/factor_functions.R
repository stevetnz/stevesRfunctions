#' Convert a logical vector to a two-level factor
#'
#' Converts a logical vector to a factor conatining two levels.
#' This is useful in statistical models such as logistic regression,
#' both for the dependent variable and the explanatory variables.
#'
#' @param bool A logical vector, or a vector that can be coerced to logical.
#' @param ifFALSE factor level for FALSE values, defaults to "No".
#' @param ifTRUE factor level for TRUE values, defaults to "Yes".
#' @value A factor with two levels.
#' @examples
#' table(NoYes(LETTERS %in% c('A','E','I','O','U')))
#' table(NoYes(rnorm(100) > 0.5))
#' @export
NoYes = function(bool, ifFALSE='No', ifTRUE='Yes') {
  if(!is.logical(bool)) bool = as.logical(bool)
  factor(ifelse(bool, ifTRUE, ifFALSE), levels=c(ifFALSE, ifTRUE))
}
