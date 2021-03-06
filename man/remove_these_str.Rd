% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_these_str.R
\name{remove_these_str}
\alias{remove_these_str}
\title{Remove these strings}
\usage{
remove_these_str(.str, expunge, ignore_case = FALSE)
}
\arguments{
\item{.str}{character vector that have tokens removed that match
tokens within the \code{expunge} vector.}

\item{expunge}{character vector of tokens to be removed from the
\code{.str} vector if present.}

\item{ignore_case}{boolean that determines whether or not case is ignored.
Defaults to FALSE.}
}
\description{
Modified from rmsutilityr::remove_strings() by R. Mark Sharp. The
modification was to remove a package dependency using the standard
relational opporator "==" instead of stri_detect_regex().
}
