## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(rmsutilityr)
library(stringi)
library(kableExtra)
library(png)
library(xtable)
options(kableExtra.auto_format = FALSE)
knitr::opts_chunk$set(echo = TRUE, include = TRUE, eval = FALSE)


## ----example-comments-1-------------------------------------------------------
#  `<!-- This is a comment without a useful label -->`
#  `<!-- RMS This is a one line comment that has my intitials as its label.-->`
#  `<!--
#  RMS Comments can span multiple lines and later can be entirely removed by
#  either selecting to remove all comments or only those comments that have
#  labels you provide as a character vector to the "label" parameter
#  -->`

## ----suggestion-1-------------------------------------------------------------
#    xt_print(all_unusual_wts_sex,
#             caption = stri_c(common_name, ":  ", animal_count, " ", sex_str,
#                             " with weights outside the ",
#                             signif(conf_int * 100, 3),
#                             "\\% predicted range."),
#             label = stri_c("tbl:", arc_species_code, "-", sex),
#             format.args = list(big.mark = ",", decimal.mark = "."),
#             size = size)
#  
#  `<!-- RMS I added some formating code (format.args) in the call to xt_print
#       please look at the output to see whether or not you prefer that formating
#       decision.-->`
#  

## ----suggestion-2-------------------------------------------------------------
#  `<!-- RMS I added some formating code (format.args) in the call to xt_print
#       please look at the output to see whether or not you prefer that formating
#       decision.
#       *** TJH I like the change as some of the numbers are over 10^5
#   -->`
#  

## ----suggestion-3-------------------------------------------------------------
#  `<!-- RMS I added some formating code (format.args) in the call to xt_print
#       please look at the output to see whether or not you prefer that formating
#       decision.
#   -->`
#  `<!-- TJH I like the change as some of the numbers are over 10^5
#   -->`
#  

## ----review-1-----------------------------------------------------------------
#    xt_print(bad_sample_dates_df, caption = "Bad Sample Dates",
#             label = "tbl:bad-sample-dates", type = type, ...)
#  
#  
#  `<!-- RMS The caption reads like a title. Consider using a caption that allows
#  the table to be understood without forcing the reader to go find the
#  narrative that describes the table contents.
#  -->`
#  

## ----review-2-----------------------------------------------------------------
#      if (nrow(bad_sample_dates_df) == 1) {
#        caption <- stri_c(
#          "There was 1 bad sample date identified where the animal was not
#          present on the date indicated.")
#      } else {
#        caption <- stri_c(
#          "There were ", nrow(bad_sample_dates_df), " bad sample dates identified
#          where the animals were not present on the dates indicated.")
#      }
#  
#  
#      xt_print(bad_sample_dates_df, caption = caption,
#               label = "tbl:bad-sample-dates", type = type, ...)
#  
#  
#  
#  `<!-- RMS The caption reads like a title. Consider using a caption that allows
#  the table to be understood without forcing the reader to go find the
#  narrative that describes the table contents.
#  *** TJH I added a more helpful dynamically generated caption so that the text
#  reflects the number of bad dates shown.
#  -->`
#  

## ----review-3-----------------------------------------------------------------
#  `<!-- RMS The caption reads like a title. Consider using a caption that allows
#  the table to be understood without forcing the reader to go find the
#  narrative that describes the table contents.
#  *** TJH I added a more helpful dynamically generated caption so that the text
#  reflects the number of bad dates shown.
#  *** RMS Nicely done. I like the dynamically generated caption.
#  accepted 20210215
#  -->`
#  

## ----get-all-comments-from-file, eval = TRUE, results = "markdown"------------
files = system.file("testdata","find_html_comment_test_file.Rmd", 
                       package = "rmsutilityr")
html_comment_lines_and_labels <- 
  get_html_comment_text_lines_and_labels_from_files(files)

caption <- 
  knitr:::escape_latex(stri_c("Output of the ", 
         "get_html_comment_text_lines_and_labels_from_files ",
         "function includes all comments when no 'label' parameter is ", 
         "provided. The columns include the file name (no path), ",
         "the possible label, the line number where the comment ",
         "starts, and the line number where the comment ends."))


 kbl(html_comment_lines_and_labels[ , c("file", "comment_label", 
                                            "comment_start_line", 
                                            "comment_end_line")],
     format = ifelse(knitr::is_latex_output(), "latex", "html"),
     longtable = TRUE, booktabs = TRUE,
     caption = caption,
     row.names = FALSE,
     col.names = c("File", "Label", "Start", "End")) %>%
   kable_styling(latex_options = c("repeat_header", "striped"), 
                 font_size = ifelse(knitr::is_latex_output(), 8, 12))


## ----get-selected-comment-text-from-file, eval = TRUE, results = "markdown"----
files = system.file("testdata","find_html_comment_test_file.Rmd", 
                       package = "rmsutilityr")
html_comment_lines_and_labels <- 
  get_html_comment_text_lines_and_labels_from_files(files, label = "RMS")

caption <- 
  knitr:::escape_latex(stri_c("Output of the ", 
         "get_html_comment_text_lines_and_labels_from_files ",
         "function includes text of comments from selected ",
         "comment labels."))


 kbl(html_comment_lines_and_labels[ , c("file", "comment_label",
                                        "comment_start_line",
                                        "comment_text")],
     format = ifelse(knitr::is_latex_output(), "latex", "html"),
     booktabs = TRUE, caption = caption,
     row.names = FALSE,
     col.names = c("File", "Label", "Start", "Text"),
     longtable = TRUE) %>% 
   kable_styling(latex_options = c("repeat_header", "striped"), 
                 font_size = ifelse(knitr::is_latex_output(), 8, 12)) %>%
   column_spec(1, width = "15em") %>%
   column_spec(2, width = "5em") %>%
   column_spec(3, width = "5em") %>%
   column_spec(4, width = "25em") 


## ----remove-selected-collaborators-comments, eval = TRUE, results = "markdown"----
files = system.file("testdata","find_html_comment_test_file.Rmd", 
                   package = "rmsutilityr")
new_files <- 
  write_files_after_deleting_selected_comments(new_path = tempdir(), 
                                               files, label = "RMS")
new_files


## ----count-of-selected-comments, eval = TRUE----------------------------------
count_selected_comments <- function(files, label = "") {
  comment_count <- 0
  for (file in files) {
    lines <- readLines(file)
    lines_and_labels <- return_html_comment_text_lines_and_labels(lines, label)
    comment_count <- comment_count + length(lines_and_labels[[1]])
  }
  comment_count
}


## ----selective-count-original-files, eval = TRUE, results = "asis"------------
count_selected_comments(files, label = "RMS")


## ----selective-count-new-files, eval = TRUE, results = "asis"-----------------
count_selected_comments(new_files, label = "RMS")


## ----selective-count-of-TJH-comments, eval = TRUE, results = "asis"-----------
count_selected_comments(new_files, label = "TJH")


## ----use-Rdiff-to-compare-files, eval = TRUE, results = "asis"----------------
tools::Rdiff(from = files[1], to = new_files[1], useDiff = TRUE, Log = TRUE)


## ----use-diffr-to-compare-files, eval = FALSE---------------------------------
#  library(diffr)
#  diffr(files[1], new_files[1])
#  

## ----example-diffr, eval = TRUE, fig.width = 6.5, echo = FALSE----------------
img <- png::readPNG("./diffr_example.png")
grid::grid.raster(img)


