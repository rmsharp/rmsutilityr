context("test_display_selected_code_comments")
## Copyright(c) 2021 R. Mark Sharp
## This file is part of rmsutilityr
files = system.file("testdata", "find_html_comment_test_file_1.Rmd",
                    package = "rmsutilityr")
tempF <- tempfile("output.txt")
sink(tempF)
output_test <-
  structure(
    "<table class=\"table\" style=\"font-size: 12px; margin-left: auto; margin-right: auto;\">\n<caption style=\"font-size: initial !important;\">Output of the get\\_html\\_comment\\_text\\_lines\\_and\\_labels\\_from\\_files function includes text of comments from selected comment labels.</caption>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\"> File </th>\n   <th style=\"text-align:left;\"> Label </th>\n   <th style=\"text-align:right;\"> Start </th>\n   <th style=\"text-align:left;\"> Text </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_1.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 16 </td>\n   <td style=\"text-align:left;width: 25em; \"> This is a comment that I want to notice later. To help others see it, I decided to write more words than are needed. </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_1.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 30 </td>\n   <td style=\"text-align:left;width: 25em; \"> Let's count this one </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_1.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 34 </td>\n   <td style=\"text-align:left;width: 25em; \"> I am counting this too. </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_2.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 16 </td>\n   <td style=\"text-align:left;width: 25em; \"> This is a comment that I want to notice later. To help others see it, I decided to write more words than are needed. </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_2.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 30 </td>\n   <td style=\"text-align:left;width: 25em; \"> Let's count this one </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;width: 15em; \"> find_html_comment_test_file_2.Rmd </td>\n   <td style=\"text-align:left;width: 5em; \"> RMS </td>\n   <td style=\"text-align:right;width: 5em; \"> 34 </td>\n   <td style=\"text-align:left;width: 25em; \"> I am counting this too. </td>\n  </tr>\n</tbody>\n</table>",
    format = "html",
    class = c("kableExtra",
              "knitr_kable")
  )
unlink(tempF)

output <- display_selected_code_comments(path = dirname(files),
                                           pattern = "Rmd",
                                           label = "RMS")
test_that("display_selected_code_comments displays the correct comments", {
  expect_equal(dput(output), output_test)
  
})
