#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_regex_all(strings, patterns, ignore_na, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See the \emph{Extended Regular Expressions} section of regex
#' for details. See fixed, ignore.case and perl for how to use other
#' types of matching: fixed, case insensitive and perl-compatible.
#' @param ignore_na if TRUE NA values are trimmed out of \code{strings} and 
#' \code{patterns} before comparison
#' @param ... further arguments for stri_detect_regex
#' @import stringi
#' @export
str_detect_regex_all <- function(strings, patterns, ignore_na, ...) {
  if (ignore_na) {
    strings <- strings[!is.na(strings)]
    patterns <- patterns[!is.na(patterns)]
  }
  sapply(patterns, function(pattern) {
    any(stri_detect_regex(strings, pattern, ...))})
}
