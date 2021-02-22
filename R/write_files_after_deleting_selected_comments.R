#' Write files after deleting selected comments.
#' 
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
#' @param new_path Character vector of length one indicating the directory
#' newly created files are to be placed.
#' @param files Character vector of files to parse.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @param isolated Logical value indicating that the comment is the first text
#' to appear on the line.
#' @export
write_files_after_deleting_selected_comments <- 
  function(new_path, files, label = "", isolated = TRUE) {
  if (!file.exists(new_path)) {
    dir.create(new_path)
    if (!file.exists(new_path)) {
      stop(paste0("Could not create directory '", new_path, "'."))
    }
  }
  new_files <- character(0)
  for (file in files) {
    lines <- delete_selected_comments_from_lines(
      readLines(file), label = label, isolated = isolated)
    new_file <- file.path(new_path, basename(file))
    writeLines(lines, con = new_file, sep = "\n", useBytes = FALSE)
    new_files <- c(new_files, new_file)
  }
  new_files
}
