#' Takes a dataframe and trims all character columns of leading and trailing
#' blanks.
#'
#' @return dataframe with values in all character columns having been
#' trimmed on both sides.
#' @param .df dataframe to have columns trimmed
#' @param cols optional character vector arguement specifying which columns to
#' trim
#' @import stringi
#' @export
stri_trim_cols <- function(.df, cols = names(.df)) {
  for (col in cols) {
    .df[ , col] <- stri_trim_both(.df[ , col])
  }
  return(.df)
}
