#' Returns a character vector with an Excel complient name having the date prepended.
#'
#' @param data_set_name character vector with name to use in file name
#' @export
get_dated_excel_name <- function(data_set_name) {
  get_dated_filename(stri_c(data_set_name, ".xlsx"))
}
