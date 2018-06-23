#' Convert underscore text strings such as \code{trans_col_select} to
#' \code{transColSelect}.
#'
#' @param s character vector that may have tokens with embedded
#' underscores.
#' @import stringi
#' @export
underscoreToCamelCase <- function(s) {
  sapply(s, function(s1) {
    s2 <- stri_split_fixed(s1, pattern = "_")[[1]]
    s3 <- stri_c(toupper(stri_sub(s2, 1, 1)),
                 stri_sub(s2, 2), collapse = "")
    stri_c(tolower(stri_sub(s3, 1, 1)),
           stri_sub(s3, 2), collapse = "")})
}
