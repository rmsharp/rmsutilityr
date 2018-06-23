#' Returns list of lists made by call to fromJSON()
#'
#' @param json_txt character vector of length 1 having one or more
#' JSON objects in text form.
#' @import jsonlite
#' @import stringi
#' @export
get_json_list <- function (json_txt) {
  t_json_txt <- json_txt
  i <- 0
  json_list <- list()
  repeat{
    i <- i + 1
    message_remainder <- get_first_json_message(t_json_txt)
    json_list[i] <- list(fromJSON(message_remainder$object))
    if (message_remainder$remainder == "")
      break
    t_json_txt <- message_remainder$remainder
  }
  json_list
}
