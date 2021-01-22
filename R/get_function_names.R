#' Get functions defined in a vector of lines
#'
#' @return A character vector of function names or an empty character vector
#'
#' @description Ignores all comment lines and defines a function name to be
#' the set of contiguous (non-blank characters) immediately prior to
#' `function\ *(` and must be the first non-blank token on a line.
#'
#' @param lines character vector with text from file
#' @importFrom stringi stri_detect_regex stri_extract_all_words 
#' @importFrom stringi stri_detect_fixed
#' @export
get_function_names <- function(lines) {
  function_names <- character(0)
  for (line in lines) {
    if (stri_detect_regex(line, pattern = "^#")) {
      next
    }
    words <- stri_extract_all_words(line)
    if (is.na(words[[1]][1]))
      next
    if (any(stri_detect_fixed(words[[1]], pattern = "function"))) {
      if (length(words[[1]]) > 1) {
        if (stri_detect_fixed(words[[1]][2], pattern = "function"))
          function_names <- c(function_names, words[[1]][1])
      } else {
        warningCondition(paste0("The line: '", line, "' has the word ",
                                "`function` and the function name was ",
                                "not found."))
      }
    }
  }
  function_names
}
