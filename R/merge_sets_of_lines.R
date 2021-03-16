#' is_html_comment_start_tag <- function(lines) {
#'   stri_detect_regex(lines, 
#'                     pattern = "(?=<!--[\\s]*)")
#' }
#' is_html_comment_end_tag <- function(line) {
#'   stri_detect_regex(line, pattern = "[\\s\\S]*?-->")[1]
#' }
#' #' Merge possible muliple lines of delimited text into a single character string.
#' #' 
#' ## Copyright(c) 2021 R. Mark Sharp
#' ## This file is part of rmsutilityr
#' #' The text being combined may be one or more lines of any type for which 
#' #' functions that can detect the starting lines and ending lines is possible.
#' #' The examples shown are for HTML comment lines as can be used in RMarkdown 
#' #' documents and example code from \code{roxygen2} function documentation 
#' #' comments.
#' #' @examples
#' #' library(rmsutilityr)
#' file <- system.file("testdata","find_html_comment_test_file_1.Rmd", 
#'                              package = "rmsutilityr")
#' 
#' lines <- readLines(file)                     
#'    html_comments <- 
#'      merge_sets_of_lines(files, is_start = is_html_comment_start_tag,
#'                          is_end = is_html_comment_end_tag)
#'    is_html_comment_start_tag <- function(lines) {
#'      stri_detect_regex(lines, 
#'                        pattern = "(?=<!--[\\s]*)")
#'    }
#'    is_html_comment_end_tag <- function(line) {
#'      stri_detect_regex(line, pattern = "[\\s\\S]*?-->")[1]
#'    }
#'    lines <- readLines(file)
#' #'                    
#' #'                    
#' #' lines <- rmsutilityr
#' #' @param lines Character vector of lines to examine.
#' #' @param isolated Logical value indicating that the comment is the first text
#' #' to appear on the line.
#' #' @export
#' merge_sets_of_lines <- function(lines, is_start = is_html_start_tag,
#'                                 is_end = is_html_end_tag,
#'                                      isolated = isolated) {
#'   set_end_line <- function(i, start_lines, n_lines) {
#'     if (start_line < start_lines[length(start_lines)])
#'       j <- min(start_lines[i + 1] - 1, n_lines)
#'     else
#'       j <- n_lines
#'     j
#'   }
#'   start_lines <- seq_along(lines)[is_start(lines)]
#'   text <- character(length(start_lines))
#'   text_line_num <- 0
#'   end_lines <- integer(0)
#'   for (i in seq_along(start_lines)) {
#'     start_line <- start_lines[i]
#'     end_line <- set_end_line(i, start_lines, length(lines))
#'     text_line_num <- text_line_num + 1
#'     
#'     for (line_num in start_line:end_line) {
#'       conditional_space <- ""
#'       if (stri_length(text[text_line_num]) > 0) {
#'         conditional_space <- 
#'           ifelse(stri_sub(text[text_line_num], -1, -1) == " " |
#'                    stri_sub(lines[line_num], 1, 1) == " ", "", " ")
#'       }
#'       
#'       text[text_line_num] <- paste0(text[text_line_num], conditional_space,
#'                                       lines[line_num])
#'       if (is_end(lines[line_num])) {
#'         break
#'       }
#'     } 
#'     end_lines <- c(end_lines, line_num)
#'   }
#'   list(text = text, start_line = start_lines, end_line = end_lines)
#' }
