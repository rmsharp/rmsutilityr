#' Detects regular expressions or fixed string values in character vectors.
#'
#' Returns a logical vector with results of stri_detect_any(), which checks 
#' for the presence of any of the elements in \code{pattern} for each element 
#' of \code{strings}.
#'
#' @usage str_detect_any(strings, patterns, type, ...)
#' @param strings input vector. This must be an atomic vector and will be
#' coerced to a character vector.
#' @param patterns patterns to look for, as defined by a POSIX regular
#' expression. See the \emph{Extended Regular Expressions} section of regex
#' for details. See fixed, ignore.case and perl for how to use other
#' types of matching: fixed, case insensitive and perl-compatible.
#' @param type character vector of length one with either
#' \code{``fixed''} or \code{``regex''}. Defaults to "fixed".
#' @param ... further arguments for stri_detect_regex
#' @importFrom stringi stri_detect_regex stri_detect_fixed
#' @export
str_detect_any <- function(strings, patterns, type = "fixed", ...) {
  sapply(seq_along(strings), function(i) {
    if (type == "regex") {
      any(stri_detect_regex(strings[i], patterns, ...))
    } else if (type == "fixed") {
      any(stri_detect_fixed(strings[i], patterns, ...))
    } else {
      stop("type must be either 'regex' or 'fixed'.")
    }
  })
}
