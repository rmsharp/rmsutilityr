#' Get labels and line numbers from HTML comments
#' 
#' @returns List with `comment_lines`, which is an integer vector of line 
#' numbers indicating which lines are comments and `comment_labels`, which is 
#' a character vector with each comment label.
#' @param lines Lines of text that may have comments.
#' @param label Optional regex expression that can be used to limit the 
#' comments found by adding each element of the character vector in turn 
#' immediately after "<!--" in the regex expression. The resulting logical
#' vectors are OR'd together to combine their results.
#' 
#' @examples 
#' return_html_comment_text_lines_and_labels(c("<!-- label1 comments -->", 
#'                                             "<!-- -->"))
#' return_html_comment_text_lines_and_labels(c("<!-- label1 comments -->", 
#'                                             "<!-- label2 comments-->"))
#' return_html_comment_text_lines_and_labels(c("<!-- label1 comments -->", 
#'                                             "<!-- label2 comments-->",
#'                                             "<!-- label3 comments -->"),
#'                                           label = "label1")
#' return_html_comment_text_lines_and_labels(c("<!-- label1 first comment -->", 
#'                                             "<!-- label2 second comment-->",
#'                                             "<!-- label3 third comment -->"),
#'                                           label = c("label1", "label3"))
#' 
#' @importFrom stringi stri_extract_first_regex stri_sub stri_trim_left
#' @importFrom magrittr %>%
#' @export
return_html_comment_text_lines_and_labels <- function(lines, label = "") {
  comments <- find_html_comments(lines, label)
  labels <- stri_trim_left(comments$comment) %>%
    stri_sub(5, -1) %>%
    stri_trim_left() %>%
    stri_extract_first_regex(pattern = "[\\p{L}*\\p{N}]*")
  comment_text <- stri_trim_left(comments$comment) %>%
    stri_sub(5, -1) %>%
    stri_trim_left() %>%
    stri_replace_first_regex(pattern = "[\\p{L}*\\p{N}]*", 
                             replacement = "") %>%
    stri_replace_last_fixed(pattern = "-->", replacement = "") %>%
    stri_trim_both()
  list(comment_text = comment_text,
       comment_start_line = comments$start_line,
       comment_end_line = comments$end_line,
       comment_label = labels)
}
