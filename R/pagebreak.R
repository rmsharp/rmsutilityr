#' pagebreak that works for PDF (LaTeX) and HTML output from \code{knitr}
#' 
#' @return Returns either a LaTeX command, \code{\\newpage}, if 
#' \code{is_latex_output()} is \code{TRUE}, an HTML command,
#' \code{<div style="page-break-before: always;" />} if 
#' \code{is_html_output()} is \code{TRUE}, and \code{NULL} if 
#' neither is \code{TRUE}.
#' 
#' @examples 
#' pagebreak()
#' @importFrom knitr is_latex_output is_html_output
#' @export
pagebreak <- function() {
  if (is_latex_output())
    return("\\newpage")
  else if (is_html_output())
    return('<div style="page-break-before: always;" />')
}
