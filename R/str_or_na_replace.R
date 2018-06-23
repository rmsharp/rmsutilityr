#' Replaces string or NA in a character vector with either NA or
#' another string.
#'
#' This is to augment \code{stri_replace_all_fixed()}, which does
#' not allow \code{NA} as a pattern.
#'
#' @param x character vector
#' @param pattern string pattern to search for may be empty string or NA.
#' Comparison is done with "==". Case can be ignored.
#' @param replacement string to replace entire entity that has pattern.
#' @param fold logical value where if TRUE the vector \code{x} and
#' \code{pattern} are converted to lower case.
#' @import stringi
#' @export
str_or_na_replace <- function(x, pattern, replacement, fold = FALSE) {
  if (is.na(pattern)) {
    x[is.na(x)] <- replacement
  } else if (fold) {
    x[tolower(x) == tolower(pattern)] <- replacement
  } else {
    x[x == pattern] <- replacement
  }
  x
}
