#' Remove these strings
#'
#' Modified from rmsutilityr::remove_strings() by R. Mark Sharp. The
#' modification was to remove a package dependency using the standard
#' relational opporator "==" instead of stri_detect_regex().
#' @param .str character vector that have tokens removed that match
#' tokens within the \code{expunge} vector.
#' @param expunge character vector of tokens to be removed from the
#' \code{.str} vector if present.
#' @param ignore_case boolean that determines whether or not case is ignored.
#' Defaults to FALSE.
#' @export
remove_these_str <- function(.str, expunge, ignore_case = FALSE) {
  if (ignore_case) {
    tmp_str <- tolower(.str)
    tmp_expunge <- tolower(expunge)
  }
  else {
    tmp_str <- .str
    tmp_expunge <- expunge
  }
  keep <- rep(TRUE, length(.str))
  for (exp_str in tmp_expunge) {
    keep <- !tmp_str == exp_str & keep
  }
  .str[keep]
}
