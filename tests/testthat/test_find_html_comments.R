context("test_find_html_comments")
library(testthat)

lines <- c("<!-- single line html comment -->",
           "<!-- this is my blog: <mynixworld.inf> -->",
           "<!--[if !(IE 8) ]><!-->",
           "<!--
            multi line html comment
            -->   ",
           "fkdshfks khh fdsfsk 
           <!--g1-->
           <div class='codetop'>CODE: AutoIt</div>
           <div class='geshimain'>
           <!--eg1-->
           <div class=\"autoit\" style=\"font-family:monospace;\">
           <span class=\"kw3\">msgbox</span>
           </div>
           <!--gc2-->
           <!--bXNnYm94-->
           <!--egc2-->
           <!--g2-->
           </div>
           <!--eg2-->
           fdsfdskh")
test_that("find_html_comment returns correct logical values", {
  expect_equal(find_html_comment(lines), "")
})

