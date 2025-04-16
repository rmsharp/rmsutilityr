#' Returns a character string of xtable format statements split up from a format
#' statement based on the number of centimeters indicated by the parameter cm
#'
#' @examples
#' library(rmsutilityr)
#' library(stringi)
#' fmt <- stri_c("p{1cm}p{2.5cm}p{1cm}p{1cm}p{2cm}p{1cm}p{6cm}p{1cm}p{2cm}")
#' split_fmt <- get_split_format(fmt, cm = 15)
#' 
#' @param fmt source format statement
#' @param cm number of centimeters format statement are to fit within
#' @export
get_split_format <- function(fmt, cm = 15) {
  col_widths_txt <- unlist(stri_extract_all_regex(fmt,
                                                  pattern = "[0-9]+\\.*[0-9]*"))
  col_widths <- as.numeric(col_widths_txt)
  ncols <- length(col_widths)
  n_page_widths <- 100 # more than we need
  split_format <- character(n_page_widths)
  cols <- matrix(rep(c(0, 0), n_page_widths), ncol = 2)
  start <- 1
  ## Space between columns:
  ## To tweak the space between columns (LaTeX will by default choose very
  ## tight columns), one can alter the column separation:
  ## \setlength{\tabcolsep}{5pt}. The default value is 6pt.
  space_between <- 0.211666667 # 6pt in cm
  
  for (page in 1:n_page_widths) {
    for (col in start:ncols) {
      if (sum(c(col_widths[start:col], space_between * (col - start))) > cm) {
        col <- col - 1
        break
      }
    }
    split_format[page] <- get_col_width_fmt(col_widths_txt[start:col])
    cols[page, ] <- c(start, col)
    if (col < ncols) {
      start <- col + 1
    } else {
      break
    }
  }
  list(split_format = split_format[1:page],
       cols = as.matrix(cols[1:page, ], ncol = 2))
}
