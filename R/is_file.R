#' Are these files?
#'
#' If at least one of the character strings is a file it returns
#' \code{exist} as a list element set to \code{TRUE} and all of the
#' character strings that are file names are retained and the
#' character strings that are not file names are removed from the
#' \code{files} character vector in the returned list.
#' If none of the  character strings is a file it returns
#' \code{exist} as a list element set to \code{FALSE}
#' and all of the character strings are retained in the
#' \code{files} character vector in the returned list.
#' @param files character vector of file names or path.
#' @param no_files_stop logical vector of length one when set to
#' \code{TRUE} the routine calls \code{stop} if no files are found.
#' @import stringi
#' @importFrom tools file_ext
is_file <- function(files, no_files_stop = FALSE) {
  files <- files[tolower(file_ext(files)) %in% c("r", "rmd", "rnw")]
  file_exist <- file.exists(files)
  if (any(file_exist)) {
    exist <- TRUE
    not_found <- files[!file_exist]
    if (length(not_found) > 0) {
      warning(stri_c("File(s) not found: ", get_and_or_list(not_found), "."))
      files <- files[file_exist]
    }
  } else if (no_files_stop) {
    stop("No files found.")
  } else {
    exist <- FALSE
  }
  list(exist = exist, files = files)
}
