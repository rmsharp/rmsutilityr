#' Returns a character vector with the remove_str pattern removed if it existed.

#' Warning: depricated; left in for compatibility. Use remove_strings() instead.
#' @param strings character vector
#' @param remove_str character vector of length one containing the regular
#' expression of the character string to be removed from the strings character
#' vector
#' @import stringi
#' @export
remove_string <- function(strings, remove_str) {
  warning(stri_c("Warning: depricated; left in for compatibility. ",
                 "Use remove_strings() instead."))
  strings[!stri_detect(strings, regex = remove_str)]
}
