% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myKnit.R
\name{myKnit}
\alias{myKnit}
\title{Custom `knit` button for RMarkdown to support colating multiple YAML header 
files before prepending to RMarkdown document}
\usage{
myKnit(
  inputFile,
  encoding,
  output_dir = output_dir,
  yamlFiles = c("common.yaml", "name.yaml")
)
}
\arguments{
\item{inputFile}{The file name of the RMarkdown file}

\item{encoding}{The `encoding` argument for `rmarkdown::render()`}

\item{output_dir}{Character vector of length one having the directory 
destination for the output file.}

\item{yamlFiles}{Character vector of relative file paths to YAML files
to be used}
}
\description{
This funtion can be called from within the YAML header of the RMarkdown
document to pull in multiple YAML source files so that common YAML
definitions can be shared amoung documents.
}
\details{
This function was adapted from the first answer, option 2 provided at
https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document/39909079#39909079?newreg=1ab151a7b48b450589dce8595951a3b7
by stackoverflow user:913184 (
https://stackoverflow.com/users/913184/mathematical-coffee)
}
