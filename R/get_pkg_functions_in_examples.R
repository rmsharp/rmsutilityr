#' Get a data.frame with a list of package functions used within the 
#' \code{examples} sections of the source code.
#' 
#' @return a data.frame containing three columns \code{path} with the full path
#' name of the source file, \code{file} with the basename of the code file, and
#' \code{function_names} with the name of the function used in the 
#' \code{examples} section of the documentation and are also functions 
#' defined within the package.
#' 
#' @param path character vector of length one having the full path name.
#' @export
get_pkg_functions_in_examples <- function(path = ".") {
  files <- get_target_files(path = path, pattern = "@examples")
  functions_in_examples <- data.frame()
  for (file in files) {
    lines <- get_example_lines(file = file)
    possible_functions <- get_files_and_function_names("R")$function_names
    found <- str_detect_regex_all(strings = lines,
                                  patterns = possible_functions,
                                  ignore_na = TRUE)
    function_names <- possible_functions[found]
    if (length(function_names) > 0) {
      functions_in_examples <-
        rbind(functions_in_examples,
              data.frame(path = rep(file, length(function_names)),
                         file = rep(basename(file), length(function_names)),
                         function_name = function_names,
                         stringsAsFactors = FALSE))
    }
  }
  functions_in_examples
}
