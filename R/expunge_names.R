#' Returns a character vector with the string pattern
#' within the expunge_str removed from names_str if they existed.
#' @param names_str character vector with names to be filtered
#' @param expunge_str character vector with names to be expunged
#' @param lower logical TRUE if names_str is to be turned to lower case
#' @export
expunge_names <- function(names_str, expunge_str = c(), lower = TRUE) {
  if (lower)
    names_str <- tolower(names_str)
  
  for (expunge_string in expunge_str) {
    names_str <- remove_string(names_str, expunge_string)
  }
  names_str
}
