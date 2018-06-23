#' \code{captitalize_first_letter()} returns character string with first
#'  character of each word capitalized and
#' remaining characters either left alone or are forced to lower case.
#'
#' @examples
#' library(stringi, quietly = TRUE)
#' capitalize_first_letter(words = c("a", "vector of words",
#'                                  "TO BE CAPITALIZED."))
#' capitalize_first_letter(words = c("a", "vector of words",
#'                                  "TO BE CAPITALIZED."), lower = TRUE)
#' @param words character string with one or more words in each element.
#' @param lower logical indicating whether remaining characters will be
#' forced to lower case.
#'
#' @import stringi
#' @export
capitalize_first_letter <- function(words = "", lower = FALSE) {
  if (lower)
    words <- tolower(words)
  new <- sapply(words, function(word) {
    word <- unlist(stri_split(as.character(word[[1]]), fixed = " "))
    substring(word, 1, 1) <- toupper(substring(word, 1, 1))
    substring(word, 2) <- substring(word, 2)
    word <- stri_c(word, collapse = " ")
    word
  })
  names(new) <- NULL
  new
}
