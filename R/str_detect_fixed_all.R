#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_fixed_all(strings, patterns, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See fixed, ignore.case and perl sections
#' for details. See  \emph{Extended Regular Expressions} for how to use regular
#' expressions for matching.
#' @param ... further arguments for stri_detect_fixed
#' @import stringi
#' @export
str_detect_fixed_all <- function(strings, patterns, ...) {
  sapply(patterns, function(pattern) {
    any(stri_detect_fixed(strings, pattern, ...))})
}
