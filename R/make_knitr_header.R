#' Assumes the presence of the LaTeX package \code{titling} with the new command
#' \code{\\subtitle}.
#' \\usepackage{titling}
#' \\newcommand{\\subtitle}[1]{%
#'   \\posttitle{%
#'     \\par\\end{center}
#'     \\begin{center}\\large#1\\end{center}
#'     \\vskip0.5em}%
#' }
#' Capitalization is not modified by this function.
#'
#' @param title_str character vector of length 1 having the text of the title
#' @param sub_title_str character vector of length 1 having the text of the
#' subtitle
#' @param author_str character vector of length 1 having the text describing the
#' author(s). Has default.
#' @export
make_knitr_header <- function(title_str = "", sub_title_str = "",
                              author_str = "Data Science Core") {
  cat(stri_c(
    "\\title{", title_str, "}\n",
    "\\subtitle{", sub_title_str, "}\n",
    "\\author{", author_str, "}\n",
    "\\date{\\today}\n",
    "\\begin{document}\n",
    "\\maketitle\n"))
}
