#' Returns character vector where each element has had its character
#' order rerversed.
#'
#' @param x - Character vector
#' @examples
#' str_reverse("abc") # "cba"
#' str_reverse(c("abc", "defgh", "i", "jklm")) # "cba" "hgfed" "i" "mlkj"
#' @import stringi
#' @export
str_reverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), stri_c,
                                  collapse = "")
