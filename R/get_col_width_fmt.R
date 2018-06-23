#' Returns a format statement used by xtable based on centimeter column widths
#'
#' @param col_widths_txt character vector with the representations of the
#' column widths to be used in the format statement.
#' @import stringi
#' @export
get_col_width_fmt <- function(col_widths_txt) {
  col_width_fmt <- "p{0cm}"
  for (txt in col_widths_txt)
    col_width_fmt <- c(col_width_fmt, stri_c("p{", txt, "cm}"))
  stri_c(col_width_fmt, collapse = "")
}
