#library(rmsutilityr)
#library(xtable)
#library(kableExtra)
#
#files <- system.file("testdata","find_html_comment_test_file_1.Rmd", 
#                     package = "rmsutilityr")
#
#html_comment_lines_and_labels <- 
#  get_html_comment_text_lines_and_labels_from_files(files, label = "RMS")
#
#caption <- 
#  knitr:::escape_latex(stri_c("Output of the ", 
#                              "get_html_comment_text_lines_and_labels_from_files ",
#                              "function includes text of comments from selected ",
#                              "comment labels."))
#
#
#xtable(html_comment_lines_and_labels[ , c("file", "comment_label", 
#                                          "comment_text")],
#       booktabs = TRUE, caption = caption) %>% 
#  xtable2kable() %>%
#  kable_styling(latex_options = "striped", font_size = 9) %>%
#  column_spec(4, width = "20em")
#