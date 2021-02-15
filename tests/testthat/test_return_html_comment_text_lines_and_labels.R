context("test_return_html_comment_text_lines_and_labels")

lines <- c("<!-- RMS single line html comment -->",
           "<!-- RMS this is my blog: <mynixworld.inf> -->",
           "<!-- RMS [if !(IE 8) ]><!-->",
           "<!-- RMS
            multi line html comment
            -->   ",
           "fkdshfks khh fdsfsk ",
           "<!-- RMS g1-->
           <div class='codetop'>CODE: AutoIt</div>
           <div class='geshimain'>",
           "<!-- RMS eg1-->
           <div class=\"autoit\" style=\"font-family:monospace;\">
           <span class=\"kw3\">msgbox</span>
           </div>",
           "<!-- RMS gc2-->",
           "<!--bXNnYm94-->",
           "<!-- TJH egc2-->",
           "<!-- 
           RMS g2-->
           </div>
           <!-- RMS eg2-->",
           "fdsfdskh",
           "<!-- 2ABC this is a comment -->")
test_that("return_html_comment_text_lines_and_labels returns correct labels", {
  lines_and_labels <- return_html_comment_text_lines_and_labels(lines)
  expect_equal(lines_and_labels$comment_label, 
               c(rep("RMS", 7), "bXNnYm94", "TJH",
                 "RMS", "2ABC"))
  expect_equal(lines_and_labels$comment_start_line, c(1:4, 6:11, 13))
  lines_and_labels <- return_html_comment_text_lines_and_labels(
    lines, label = c("RMS", "TJH"))
  expect_equal(lines_and_labels$comment_label, 
               c(rep("RMS", 7), "TJH",
                 "RMS"))
  expect_equal(lines_and_labels$comment_start_line, c(1:4, 6:8, 10:11))
})
