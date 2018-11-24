context("test_bcomment")

test_that("bcomment forms the correct string", {
  color <- "blue"
  test_str <- "test string"
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat))
    outputFormat <- "latex"
  if (outputFormat == "latex")
    text <- paste0("\\textcolor{", color, "}{\\emph{", test_str,"}}")
  else if (outputFormat == 'html')
    text <- paste0("<font color='", color, "'>_", test_str, "_</font>")
  else
    text <- x
  
  expect_equal(bcomment("test string"), text)
})
