#' Merge multiline HTML comments into a single character string.
#' 
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
#' @param lines Character vector of lines to examine.
#' @param isolated Logical value indicating that the comment is the first text
#' to appear on the line.
#' @export
merge_html_comment_lines <- function(lines, isolated = isolated) {
  set_end_line <- function(start_line, i, start_lines, n_lines) {
    if (start_line < start_lines[length(start_lines)])
      j <- min(start_lines[i + 1] - 1, n_lines)
    else
      j <- n_lines
    j
  }
  start_lines <- seq_along(lines)[stri_detect_regex(lines, 
                                                    pattern = "(?=<!--[\\s]*)")]
  comments <- character(length(start_lines))
  comment_num <- 0
  end_lines <- integer(0)
  for (i in seq_along(start_lines)) {
    start_line <- start_lines[i]
    end_line <- set_end_line(start_line, i, start_lines, length(lines))
    comment_num <- comment_num + 1
        
    for (line_num in start_line:end_line) {
      conditional_space <- ""
      if (stri_length(comments[comment_num]) > 0) {
        conditional_space <- 
          ifelse(stri_sub(comments[comment_num], -1, -1) == " " |
              stri_sub(lines[line_num], 1, 1) == " ", "", " ")
      }
      
      comments[comment_num] <- paste0(comments[comment_num], conditional_space,
                                      lines[line_num])
      if (stri_detect_regex(lines[line_num], pattern = "[\\s\\S]*?-->")[1]) {
        break
      }
    } 
    end_lines <- c(end_lines, line_num)
  }
  list(comment = comments, start_line = start_lines, end_line = end_lines)
}
