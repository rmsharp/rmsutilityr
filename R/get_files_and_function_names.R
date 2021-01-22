#' Get the file names and functions defined in those files
#' 
#' @return data.frame with a column of file names and the corresponding
#' `function\ *(` and must be the first non-blank token on a line.
#' @param path character vector of length one having the directory path to
#' the files containing function definitions.
#' @importFrom stringi stri_detect_regex stri_extract_all_words
#' @export
get_files_and_function_names <- function(path = ".") {
  files <- list.files(path = path, full.names = TRUE)
  files_and_function_names <- data.frame()
  for (file in files) {
    lines <- readLines(file)
    function_names <- get_function_names(lines)
    if (length(function_names) > 0) {
      files_and_function_names <- 
        rbind(files_and_function_names, 
              data.frame(file = rep(file, length(function_names)), 
                         function_names = function_names,
                         stringsAsFactors = FALSE))
    } else {
      rbind(files_and_function_names, 
            data.frame(file = file, 
                       function_names = "**No function defined**",
                       stringsAsFactors = FALSE))
    }
  }
  files_and_function_names
}
