#' Write files after deleting selected comments.
#' 
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
#' @param files Character vector of files to parse.
#' @param new_path Character vector of length one indicating the directory
#' newly created files are to be placed.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @param isolated Logical value indicating that the comment is the first text
#' to appear on the line.
#' @param overwrite logical; should existing destination files be overwritten?
#' @export
write_files_after_deleting_selected_comments <-
  function(files, new_path, label = "", isolated = TRUE, overwrite = FALSE) {
    make_new_path(new_path)
    relative_path_file_names <- 
      get_relative_path_file_names(files, new_path = new_path)
    for (i in seq_along(files)) {
      original_file <- relative_path_file_names$original_file[i]
      new_file <- relative_path_file_names$new_file[i]
      write_lnes_after_deleting_selected_comments(
        original_file, new_file, label, isolated, overwrite)
    }
    relative_path_file_names
  }
#' Makes the new path if needed
#' 
#' @returns NULL
#' 
#' Has the side effect of creating file directories if they do not exists.
#' It stops the script if it fails.
#' 
#' @param new_path Character vector of length one indicating the directory
#' newly created files are to be placed.
make_new_path <- function(new_path) {
  if (!file.exists(new_path)) {
    dir.create(new_path)
    if (!file.exists(new_path)) {
      stop(paste0("Could not create directory '", new_path, "'."))
    }
  }
}
#' Writes lines (contents of a file minuts comments) to a new file.
#' 
#' @returns NULL
#' 
#' Reads the \code{original_file} and deleats HTML comments that have the
#' content of the \code{label} detected. It then writes the modified lines
#' out to the \code{new_file}.
#' 
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @param isolated Logical value indicating that the comment is the first text
#' to appear on the line.
#' @param overwrite logical; should existing destination files be overwritten?
write_lnes_after_deleting_selected_comments <-
  function(original_file, new_file, label, isolated, overwrite) {
    lines <-
      delete_selected_comments_from_lines(readLines(original_file),
                                          label = label,
                                          isolated = isolated)
    if (overwrite | !file.exists(new_file)) {
      make_new_path(dirname(new_file))
      writeLines(lines,
                 con = new_file,
                 sep = "\n",
                 useBytes = FALSE)
    } else {
      stop(paste0(
        "File '",
        new_file,
        "' exists and you requested it not be replaced."
      ))
    }
  }
