#' Find HTML comment
#' 
#' @importFrom stringi stri_detect_regex
#' @export
find_html_comments <- function(lines) {
  stri_detect_regex(lines, pattern = "(?=<!--)([\\s\\S]*?-->) ")
}
