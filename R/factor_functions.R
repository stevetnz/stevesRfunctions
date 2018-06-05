#' Convert a logical vector to a two-level factor
#'
#' Converts a logical vector to a factor conatining two levels.
#' This is useful in statistical models such as logistic regression,
#' both for the dependent variable and the explanatory variables.
#'
#' @param bool A logical vector, or a vector that can be coerced to logical.
#' @param levels Two factor levels for FALSE and TRUE values, respectively.
#' @return A factor with two levels.
#' @examples
#' table(NoYes(LETTERS %in% c('A','E','I','O','U')))
#' table(NoYes(runif(100) > 0.5, c('Tails', 'Heads')))
#' @export
NoYes = function(bool, levels=c('No', 'Yes')) {
  if(!is.logical(bool)) bool = as.logical(bool)
  factor(bool, levels=c(FALSE,TRUE), labels=levels)
}
