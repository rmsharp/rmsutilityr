#' Returns a string formated as a comment for HTML or LaTeX depending on
#' output format.
#'
#' @param  x character string with comment.
#' @param color color that font is to be
#' @export
colfmt = function(x, color = "red") {
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(outputFormat))
    outputFormat <- "latex"
  if (outputFormat == "latex")
    paste0("\\textcolor{", color, "}{\\emph{", x,"}}")
  else if (outputFormat == 'html')
    paste0("<font color='", color, "'>_", x, "_</font>")
  else
    x
}
