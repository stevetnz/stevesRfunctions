c.factor = function(fac1, fac2) {
  factor(
    c(as.character(fac1), as.character(fac2)),
    levels=unique(c(levels(fac1), levels(fac2)))
  )
}

NoYes = function(bool) {
  if(!is.logical(bool)) bool = as.logical(bool)
  factor(ifelse(bool, "Yes", "No"), levels=c("No", "Yes"))
}
