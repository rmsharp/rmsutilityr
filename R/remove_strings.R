#' Returns a character vector with the string patterns within the expunge
#' character vector removed.
#'
#' Uses stri_detect_regex.
#' @examples
#' strngs <- c("abc", "d", "E", "Fh")
#' expunge <- c("abc", "D", "E")
#' remove_strings(strngs, expunge, ignore_case = TRUE)
#' remove_strings(strngs, expunge, ignore_case = FALSE)
#'
#' @param .str character vector
#' @param expunge character vector of strings to be removed
#' @param ignore_case logical indication whether or not to be case specific.
#' @import stringi
#' @export
remove_strings <- function(.str, expunge, ignore_case = FALSE) {
  if (ignore_case) {
    tmp_str <- tolower(.str)
    tmp_expunge <- tolower(expunge)
  } else {
    tmp_str <- .str
    tmp_expunge <- expunge
  }
  keep <- rep(TRUE, length(.str))
  for (exp_str in tmp_expunge) {
    keep <- !stri_detect_regex(tmp_str, exp_str) & keep
  }
  .str[keep]
}
