#' Convert underscore text strings such as \code{trans_col_select} to
#' \code{TransColSelect}.
#'
#' @param s character vector that may have tokens with embedded
#' underscores.
#' @import stringi
#' @export
underscoreToTitleCase <- function(s) {
  sapply(s, function(s1) {
    s2 <- stri_split_fixed(s1, pattern = "_")[[1]]
    stri_c(toupper(stri_sub(s2, 1, 1)),
           stri_sub(s2, 2), collapse = "")})
}
