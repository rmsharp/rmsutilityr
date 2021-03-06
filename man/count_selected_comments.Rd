% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_selected_comments.R
\name{count_selected_comments}
\alias{count_selected_comments}
\title{Count selected comments in files.}
\usage{
count_selected_comments(files, label = "")
}
\arguments{
\item{files}{Character vector of files to parse.}

\item{label}{Optional regex expression that can be used to limit the 
comments found by adding each element of the character vector in turn 
immediately after "<!--" in the regex expression. The resulting logical
vectors are OR'd together to combine their results.}
}
\description{
Count selected comments in files.
}
