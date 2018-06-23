#' Returns character vector of one containing the results of the Tukey
#' five number summary calculations made by \code{fivenum()}.
#'
#' @param dist vector of numbers that are the argument to \code{fivenum()}
#' @param digits integer values with the number of digits to be displayed.
#' @import stats
#' @import stringi
#' @export
latex_fivenum <- function(dist, digits = 3) {
  stri_c("{", vector2string(signif(fivenum(dist), digits), SS = "}{"), "}")
}
