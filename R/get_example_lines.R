#' Get the examples lines from a source code file.
#' 
#' @return Character vector with one lines from the 
#' \code{examples} section of the code.
#' @description User provides the source code file path. This a a simplistic
#' function that simply returns lines between \code{@examples} and the next
#' line that starts with \code{#' @}, or anything not \code{#'}.
#' 
#' @param file character vector of length 1 with the file to extract example
#' code from.
#' @importFrom stringi stri_detect_fixed
#' @export
get_example_lines <- function(file = stdin()) {
  lines <- readLines(file)
  len <- length(lines)
  if (len > 0) {
    counter <- 0
    for (line in lines) {
      counter <- counter + 1
      if (stri_detect_regex(line, pattern = "^#'\\s@examples[\\s]*"))
        break
    }
    lines <- lines[counter + 1:len]
    example_lines <- character(0)
    for (line in lines) {
      if (any(stri_detect_regex(line, pattern = "^#'\\s@")) |
          !any(stri_detect_regex(line, pattern = "^#'")))
        break
      example_lines <- c(example_lines, line)
    }
    example_lines
  }
}
