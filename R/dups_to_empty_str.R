#' Replaces duplicate strings with empty strings.
#'
#' Replaces duplicate strings with empty strings
#' in a character vector.
#'
#' @examples
#' library(stringi)
#' dups_to_empty_str(c("A", "duplicated", "duplicated", "word"))
#' @param char_vec character vector
#' @import stringi
#' @export
dups_to_empty_str <- function(char_vec) {
  if (length(char_vec) > 1) {
    element <- char_vec[1]
    for (i in 2:length(char_vec)) {
      if (element == char_vec[i]) {
        char_vec[i] <- ""
      } else {
        element <- char_vec[i]
      }
    }
  }
  char_vec
}
