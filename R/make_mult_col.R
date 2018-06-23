#' Returns dataframe with multiple sets of the original columns (cols) made
#' from the original dataframe columns.
#'
#' Thus, a 10 row 1 column dataframe becomes a 4 row 3 column dataframe with
#' 2 NA values added to fill out the end of the new dataframe.
#' This is used to reduce the length of very narrow dataframes.
#'
#' @param .df dataframe
#' @param cols number of set of columns to be made
#' @import stringi
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5], c = LETTERS[1:5])
#' make_mult_col(df, 2)
#' @export
make_mult_col <- function(.df, cols = 1) {
  n_orig_col <- ncol(.df)
  rows <- ceiling(nrow(.df) / cols)
  new_df <- data.frame(col = rep(NA, rows))
  total <- nrow(.df)
  .df <- rbind(.df, rep(NA, (rows * cols * n_orig_col) - total))
  for (i in seq(1, rows * cols, by = rows)) {
    j <- i + rows - 1
    new_df <- cbind(new_df, .df[i:j, ])
  }
  new_df <- new_df[ , -1]
  names(new_df) <- stri_c(names(.df), rep(1:cols, each = n_orig_col))
  new_df
}
