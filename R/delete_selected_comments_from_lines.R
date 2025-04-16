#' Delete selected comments from lines.
#'
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
#' @param lines Character vector of lines to examine.
#' @param label Optional regex expression that can be used to limit the
#' comments found by adding each element of the character vector in turn
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' @param isolated Logical value indicating that the comment is the first text
#' to appear on the line.
#' @importFrom stringi stri_detect_regex
#' @export
delete_selected_comments_from_lines <- function(lines, label = "",
                                                isolated = TRUE) {
  comments <- find_html_comments(lines, label)
  delete_seq <- integer(0)
  for (i in seq_along(comments$start_line)) {
    delete_seq <- c(delete_seq, 
                    comments$start_line[i]:comments$end_line[i])
  }
  lines[!seq_along(lines) %in% delete_seq]
}
