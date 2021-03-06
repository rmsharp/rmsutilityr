% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_date_str.R
\name{make_date_str}
\alias{make_date_str}
\title{Make a date character string given a date.}
\usage{
make_date_str(.date, label = TRUE, abbr = TRUE)
}
\arguments{
\item{.date}{a POSIXct or Date object to be translated}

\item{label}{(from ?lubridate:::month) logical. TRUE will display
the month as a character string such as "January."
FALSE will display the month as a number.}

\item{abbr}{(from ?lubridate:::month) logical. FALSE will display
the month as a character string #' label, such as #' "January".
TRUE will display an abbreviated version of the label,
such as "Jan". abbr is #' disregarded if label = FALSE.}
}
\description{
This function takes a date and arguments about how to represent months
to form a character string that forms a date.
}
\examples{
library(stringi, quietly = TRUE)
library(lubridate, quietly = TRUE)
make_date_str(ymd("2010/3/21", quiet = TRUE))
make_date_str(ymd("2010/3/21", quiet = TRUE), abbr = TRUE)
}
\keyword{date}
