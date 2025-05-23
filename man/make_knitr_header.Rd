% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_knitr_header.R
\name{make_knitr_header}
\alias{make_knitr_header}
\title{Assumes the presence of the LaTeX package \code{titling} with the new command}
\usage{
make_knitr_header(
  title_str = "",
  sub_title_str = "",
  author_str = "Data Science Core"
)
}
\arguments{
\item{title_str}{character vector of length 1 having the text of the title}

\item{sub_title_str}{character vector of length 1 having the text of the
subtitle}

\item{author_str}{character vector of length 1 having the text describing the
author(s). Has default.}
}
\description{
\code{\\subtitle}.
\\usepackage{titling}
\\newcommand{\\subtitle}[1]{%
  \\posttitle{%
    \\par\\end{center}
    \\begin{center}\\large#1\\end{center}
    \\vskip{0.5em}%
    }
}
}
\details{
Capitalization is not modified by this function.
}
