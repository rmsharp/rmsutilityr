#' Count selected comments in files.
#' 
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
#' @param files Character vector of files to parse.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @export
count_selected_comments <- function(files, label = "") {
  comment_count <- 0
  for (file in files) {
    lines <- readLines(file)
    lines_and_labels <- 
      return_html_comment_text_lines_and_labels(lines, label)
    comment_count <- comment_count + length(lines_and_labels[[1]])
  }
  comment_count
}
