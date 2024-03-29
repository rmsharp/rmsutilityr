% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cran_db_factory.R
\name{get_cran_db_factory}
\alias{get_cran_db_factory}
\title{Returns the CRAN database but only refreshes once a day unless forced}
\usage{
get_cran_db_factory()
}
\value{
data base from CRAN
}
\description{
An argument (\code{new_cran_db}; logical) can be used to control whether or 
not the CRAN database is refreshed each call. 
When set to TRUE the function performs a get from CRAN to get a fresh copy
of the database. Default is \code{FALSE}.
}
