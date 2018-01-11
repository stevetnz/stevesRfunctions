#' Carry forward/back imputation of a vector
#'
#' Fill in missing values by carrying forward (or back) the known values in a vector or factor.
#'
#' @param v A numeric or character vector, or a factor, containing some missing values.
#' @examples
#' carryforward(c(NA,10,NA,23,19,NA,NA,NA,0,NA))
#' carryback(c(NA,10,NA,23,19,NA,NA,NA,0,NA))
#' @export
carryforward = function(v) {
  if(!is.vector(v) & is.factor(v)) {
    warning("'v' must be a vector")
    return(v)
  }
  if (length(v)==0) return(v)
  ok = if (is.character(v)) (!is.na(v) & v != '') else (!is.na(v))
  ok[1] = TRUE
  setNames(v[ok][cumsum(ok)], names(v))
}

#' @describeIn carryforward The reverse
#' @export
carryback = function(v) rev(carryforward(rev(v)))
