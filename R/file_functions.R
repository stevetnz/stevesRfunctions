#' Write CSV file with different defaults
#'
#' Write a CSV file using \code{write.csv} (see \code{\link[utils]{write.table}}
#' with different defaults.
#' Specifically, NA values are empty, row names are excluded and the *.CSV file extention is added if not found in the file name.
#' The file name can default to a name derived from the name of the first argument.
#'
#' @usage Write.csv(x, file, ...)
#'
#' @param x Matrix or data.frame
#' @param file File name, perhaps without .CSV extention. Defaults to name of first argument.
#'
#' @examples \dontrun{
#' Write.csv(iris)
#' }
#'
#' @export
#'
Write.csv = function(x, file=deparse(substitute(x)), row.names=FALSE, na='', ...) {
  if (!grepl('[.]csv$', file, ignore=TRUE))
    file = paste0(file, '.csv')
  utils::write.csv(x, file, row.names=row.names, na=na, ...)
}
