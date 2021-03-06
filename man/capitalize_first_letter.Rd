% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capitalize_first_letter.R
\name{capitalize_first_letter}
\alias{capitalize_first_letter}
\title{Captitalize first letter of each word in a character string}
\usage{
capitalize_first_letter(words = "", lower = FALSE)
}
\arguments{
\item{words}{character string with one or more words in each element.}

\item{lower}{logical indicating whether remaining characters will be
forced to lower case.}
}
\value{
character string with first character of each word capitalized and
remaining characters either left alone or are forced to lower case.

In many cases the user will want to use \code{tools::toTitleCase} instead.
}
\description{
Captitalize first letter of each word in a character string
}
\examples{
library(stringi, quietly = TRUE)
capitalize_first_letter(words = c("a", "vector of words",
                                 "TO BE CAPITALIZED."))
capitalize_first_letter(words = c("a", "vector of words",
                                 "TO BE CAPITALIZED."), lower = TRUE)
}
