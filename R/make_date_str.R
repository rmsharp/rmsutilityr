#' Make a date character string given a date.
#'
#' This function takes a date and arguments about how to represent months
#' to form a character string that forms a date.
#'
#' @param .date a POSIXct or Date object to be translated
#' @param label (from ?lubridate:::month) logical. TRUE will display
#' the month as a character string such as "January."
#' FALSE will display the month as a number.
#' @param abbr (from ?lubridate:::month) logical. FALSE will display
#' the month as a character string #' label, such as #' "January".
#' TRUE will display an abbreviated version of the label,
#' such as "Jan". abbr is #' disregarded if label = FALSE.
#' @keywords date
#' @examples
#' library(stringi, quietly = TRUE)
#' library(lubridate, quietly = TRUE)
#' make_date_str(ymd("2010/3/21", quiet = TRUE))
#' make_date_str(ymd("2010/3/21", quiet = TRUE), abbr = TRUE)
#' @import stringi
#' @export
make_date_str <- function(.date, label = TRUE, abbr = TRUE) {
  stri_c(month(.date, label = label, abbr = abbr), " ",
         day(.date), ", ", year(.date))
}
