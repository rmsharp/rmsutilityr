#' Detects regular expressions or fixed string values in character vectors.
#'
#' Returns a logical vector with results of stri_detect() for each pattern in
#' second parameters character vector.
#'
#' @usage str_detect_all(strings, patterns, type, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See the \emph{Extended Regular Expressions} section of regex
#' for details. See fixed, ignore.case and perl for how to use other
#' types of matching: fixed, case insensitive and perl-compatible.
#' @param type character vector of length equal to \code{search} with either
#' \code{``fixed''} or \code{``regex''}.
#' @param ... further arguments for stri_detect_regex
#' @import stringi
#' @export
str_detect_all <- function(strings, patterns, type, ...) {
  sapply(seq_along(patterns), function(i) {
    if (type[i] == "regex") {
      any(stri_detect_regex(strings, patterns[i], ...))
    } else if (type[i] == "fixed") {
      any(stri_detect_fixed(strings, patterns[i], ...))
    } else {
      stop("type must be the same length as patterns and either 'regex' or
           'fixed'.")
    }
  })
}
