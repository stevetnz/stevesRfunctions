#' Create a random password.
#'
#' Generates a random password containing the desired number of letters,
#' numeric digits and ASCII symbols.
#'
#' @param Letters Number of characters from \link{letters}/LETTERS to include.
#' @param Numbers Number of digits from 0-9 to include.
#' @param Symbols Number of symbol characters to include.
#' @return A randomly generaged password.
#' @examples
#' # Defaults; easy to double-click, copy and paste.
#' newpassword()
#' # A longer and more complicated password.
#' newpassword(9,7,5)
#' # Multiple new passwords.
#' replicate(10, newpassword())
#' @export
newpassword = function(Letters=8, Numbers=5, Symbols=0) {
  pwd = c(
    sample(c(letters, LETTERS), Letters, replace=TRUE),
    sample(0:9, Numbers, replace=TRUE),
    sample(unlist(strsplit(intToUtf8(c(33,35:38,40:47,58:63,91,93:95,123:126)),"")), Symbols, replace=TRUE)
  )
  paste(sample(pwd),collapse='')  # shuffle and collapse
}
