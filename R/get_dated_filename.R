#' Returns a character vector with an file name having the date prepended.
#'
#' @param filename character vector with name to use in file name
#' @import lubridate
#' @export
get_dated_filename <- function(filename) {
  date_stamp <- stri_replace_all_fixed(
    stri_replace_all_fixed(as.character(now()), " ", "_"), ":", "_")
  stri_c(date_stamp, "_", filename)
}
