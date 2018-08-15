#' Returns TRUE if the string is a valid date.
#'
#' @examples
#' is_valid_date_str(c("13-21-1995", "20-13-98", "5-28-1014",
#'   "1-21-15", "2-13-2098", "25-28-2014"), format = "%m-%d-%y")
#'
#' @param date_str character vector with 0 or more dates
#' @param format character vector of lenght one having the date format
#' @param optional parameter to \code{as.Date}. Logical value indicating 
#' to return NA (instead of signaling an error) if the format guessing does not succeed.
#' descriptor. Defaults to FALSE.
#' @export
is_valid_date_str <- function(date_str, format = "%d-%m-%Y %H:%M:%S", optional = FALSE) {
  if (!is.character(date_str)) {
    if (is.numeric(date_str))
      return(rep(FALSE, length(date_str)))
  }
  result <- as.logical(sapply(date_str, function(s) {
    d <- try(as.Date(s, format = format, optional = optional))
    !(class(d) == "try-error" || is.na(d))
  }))
  result
}
