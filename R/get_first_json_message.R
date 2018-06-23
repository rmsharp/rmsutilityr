#' Returns a list containing two objects in the text of a character vector
#' of length one: (1) object = the first json object found and (2) remainder =
#' the remaining text.
#'
#'  Properly formed messages are assumed. Error checking is non-existent.
#' @param json_txt character vector of length one having one or more JSON
#' objects in character form.
#' @import stringi
#' @export
get_first_json_message <- function(json_txt) {
  len <- stri_length(json_txt)
  braces <- match_braces(json_txt)
  if (braces$right[1] + 1 > len) {
    remainder <- ""
  } else {
    remainder <- stri_trim_both(stri_sub(json_txt, braces$right[1] + 1))
  }
  list(object = stri_sub(json_txt, braces$left[1], to = braces$right[1]),
       remainder = remainder)
}
