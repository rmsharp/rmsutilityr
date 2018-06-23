#' Creates a list object containing number of lines, blank lines,
#' bare comments, roxygen2 documentation, and code.
#'
#' @param files character vector of file names. The file names may contain the
#' path. Without a path in the file name and without a defined \code{path}
#' paramenter, the current directory is used.
#' @param path optional character vector of length one or of the same length as
#' \code{files}. If of length one, it is used for all files. If it is the
#' same length as files, each value is paired with its corresponding file to
#' make the fully qualified name.
#' @import stringi
#' @export
classify_code_lines <- function(files = ".", path = NULL) {
  if (files[1] == "." & length(files) == 1 & is.null(path)) {
    files <- list.files(path = ".", full.names = TRUE)
  }
  if (!is.null(path)) {
    files <- stri_c(path, "/", files)
    files <- stri_replace_all_fixed(files, pattern = "//", replacement = "/")
  }
  
  file_list <- is_file(files, no_files_stop = FALSE)
  if (!file_list$exist) { # May be path argument
    files <- list.files(path = files, full.names = TRUE)
    file_list <- is_file(files, no_files_stop = TRUE)
  }
  
  count <- 0
  code <- 0
  comments <- 0
  roxygen_comments <- 0
  blank_lines <- 0
  for (file in file_list$files) {
    lines <- readLines(file)
    count <- count + length(lines)
    for (line in lines) {
      if (stri_sub(line, 1, 2) == "#'") {
        roxygen_comments <- roxygen_comments + 1
        next
      }
      line <- stri_trim_both(line)
      if (stri_length(line) == 0) {
        blank_lines <- blank_lines + 1
        next
      }
      buf <- stri_split_boundaries(line, type = "word")
      if (stri_detect_fixed(buf[[1]][1], "#")) {
        comments <- comments + 1
        next
      } else {
        code <- code + 1
      }
    }
  }
  c(files = length(file_list$files), lines = count, code = code,
    blank_lines = blank_lines, roxygen_doc_lines = roxygen_comments,
    comments = comments)
}
