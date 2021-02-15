#' Read in file and find, classify, and count comments by label type.
#' 
#' @param files Names of files
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' 
#' @export
get_html_comment_text_lines_and_labels_from_files <- function(files, label = "") {
  files_lines_labels <- data.frame()
  for (file in files) {
    lines <- readLines(file)
    lines_and_labels <- return_html_comment_text_lines_and_labels(lines, label)
    if (length(lines_and_labels$comment_start_line) > 0) {
      files_lines_labels <- 
        rbind(files_lines_labels, 
              data.frame(
                path = rep(file, length(lines_and_labels$comment_start_line)), 
                file = rep(basename(file), 
                           length(lines_and_labels$comment_start_line)), 
                comment_text = lines_and_labels$comment_text,
                comment_start_line = lines_and_labels$comment_start_line,
                comment_end_line = lines_and_labels$comment_end_line,
                comment_label = lines_and_labels$comment_label,
                stringsAsFactors = FALSE))
      files_lines_labels <- files_lines_labels[order(
        files_lines_labels$path, 
        files_lines_labels$comment_label, 
        files_lines_labels$comment_start_line), ]
      
    }  
  }
  files_lines_labels
}
