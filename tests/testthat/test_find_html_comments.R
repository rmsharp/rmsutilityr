context("test_find_html_comments")
library(testthat)

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
           "<!-- RMS g2-->
           </div>
           <!-- RMS eg2-->",
           "fdsfdskh")
test_that("find_html_comment returns correct logical values", {
  expect_equal(find_html_comments(lines), 
               c(rep(TRUE, 4), FALSE, rep(TRUE, 6), FALSE))
  expect_equal(find_html_comments(lines, label = "RMS"), 
               c(rep(TRUE, 4), FALSE, rep(TRUE, 3), FALSE, FALSE, TRUE, FALSE))
  expect_equal(find_html_comments(lines, label = "TJH"), 
               c(rep(FALSE, 9), TRUE, FALSE, FALSE))
  expect_equal(find_html_comments(lines, label = " TJH "), 
               c(rep(FALSE, 9), TRUE, FALSE, FALSE))
})

