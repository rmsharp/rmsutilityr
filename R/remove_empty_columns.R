#' Returns a dataframe after empty columns have been removed.
#'
#' @param df dataframe
#' @export
remove_empty_columns <- function(df) {
  vec <- c()
  for (i in seq_along(names(df))) {
    if (sum(abs(!is.na(df[i]))) > 0) {
      vec <- c(vec, names(df[i]))
    }
  }
  subset(df, select = vec)
}
