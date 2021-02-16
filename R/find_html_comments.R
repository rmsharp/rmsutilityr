#' Find HTML comments
#' 
#' Only comments that do not follow other text on a line are detected by 
#' default.
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
find_html_comments <- function(lines, label = "", isolated = TRUE) {
  comments_merged <- merge_html_comment_lines(lines, isolated = isolated)
  comments <- rep(FALSE, length(comments_merged$comment))
  for (pat in label) {
    pattern <- paste0("(?=<!--[\\s]*", pat, ")([\\s\\S]*?-->)")
    comments_with_pattern <- stri_detect_regex(comments_merged$comment, 
                                               pattern = pattern)
        comments <- comments | comments_with_pattern
  }
  list(comment = comments_merged$comment[comments], 
       start_line = comments_merged$start_line[comments],
       end_line = comments_merged$end_line[comments])
}
