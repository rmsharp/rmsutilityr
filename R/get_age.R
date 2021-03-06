#' Returns the fractional age in years, months or days between the start_date
#' and the end_date.
#'
#' @param start_date a POSIXct or POSIXt object with the beginning date (such
#' as a birth date).
#' @param end_date  a POSIXct or POSIXt object with the ending date (such
#' as a death date).
#' @param unit one of c("year", "month", "week", "day") that become the
#' unit used as the devisor for the time interval.
#' @importFrom lubridate dyears dweeks ddays
#' @importFrom stringi stri_c
#' @export
get_age <- function(start_date, end_date, unit) {
  if (missing(unit)) {
    cat(stri_c("You must enter a 'unit' argument that is one of 'year', ",
               "'month', 'week', 'day'.\n"))
    invisible(unit)
  }
  match.arg(unit, c("year", "month", "week", "day"))
  switch(unit,
         year = (end_date - start_date) / dyears(1),
         month = 12.0 * (end_date - start_date) / dyears(1),
         week = (end_date - start_date) / dweeks(1),
         day = (end_date - start_date) / ddays(1))
}
