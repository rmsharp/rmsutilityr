#' Returns a list of data frames split up for printing so that they do not
#' exceed the width (cm) specified.
#'
#' @param df source dataframe
#' @param fmt source format statement
#' @param cm number of centimeters format statement are to fit within
#' @export
get_split_table <- function(df, fmt, cm = 15) {
  split <- get_split_format(fmt, cm)
  split_df_list <- vector("list", length(split$split_format))
  for (page in seq_along(split$split_format)) {
    split_df_list[[page]] <-
      df[ , split$cols[page, 1]:split$cols[page, 2]]
  }
  split_df_list
}
