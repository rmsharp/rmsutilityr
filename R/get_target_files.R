#' Get a list of files based on presence of a text pattern
#' 
#' @return A character string containing file paths to targeted files.
#' @param path A character vector of length one having the path of the directory
#' to be searched.
#' @param pattern A character vector of length one having the pattern to be 
#' used for matching. Currently only fixed pattern matching is supported.
#' 
#' @importFrom stringi stri_detect_fixed
#' @export
get_target_files <- function(path, pattern) {
  files <- list.files(path = path, full.names = TRUE)
  target_files <- character(0)
  for (file in files) {
    lines <- readLines(file)
    if (any(stri_detect_fixed(lines, pattern = pattern)))
      target_files <- c(target_files, file)
  }
  target_files
}
