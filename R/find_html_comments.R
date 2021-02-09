#' Find HTML comment
#' 
#' @param lines Character vector of lines to examine.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding this character vector of length one immediately 
#' after "<!--".
#' @importFrom stringi stri_detect_regex
#' @export
find_html_comments <- function(lines, label = "") {
  pattern <- paste0("(?=<!--[\\s]*", label, ")([\\s\\S]*?-->)")
  stri_detect_regex(lines, pattern = pattern)
}
# stri_count_regex(lines, pattern = "(?=<!-- RMS)([\\s\\S]*?-->)")
# stri_locate_regex(lines, pattern = "(?=<!-- RMS)([\\s\\S]*?-->)")
# stri_extract_all(lines, pattern = "(?=<!-- RMS)([\\s\\S]*?-->)", regex = TRUE)
# new_lines <- stri_replace_all_regex(lines, pattern = "(?=<!-- RMS)([\\s\\S]*?-->)", 
#                        replacement = "")
# print(lines)
# print(new_lines)
