#' Custom `knit` button for RMarkdown to support colating multiple YAML header 
#' files before prepending to RMarkdown document
#'
#' This funtion can be called from within the YAML header of the RMarkdown
#' document to pull in multiple YAML source files so that common YAML
#' definitions can be shared amoung documents.
#'
#' This function was adapted from the first answer, option 2 provided at
#no lint start
#' https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document/39909079#39909079?newreg=1ab151a7b48b450589dce8595951a3b7
#no lint end
#' by stackoverflow user:913184 (
#' https://stackoverflow.com/users/913184/mathematical-coffee)
#'
#' @param inputFile The file name of the RMarkdown file
#' @param encoding The `encoding` argument for `rmarkdown::render()`
#' @param yamlFiles Character vector of relative file paths to YAML files
#' to be used
#' @param output_dir Character vector of length one having the directory 
#' destination for the output file.
#' @importFrom rmarkdown render
#' @export
myKnit <- function(inputFile,
                   encoding,
                   output_dir = output_dir,
                   yamlFiles = c("common.yaml", "name.yaml")) {
  # read in the YAML + src file
  yaml <- ""
  if (!is.null(yamlFiles)) {
    for (yamlFile in yamlFiles) {
      yaml <- c(yaml, readLines(yamlFile))
    }
  }
  rmd <- readLines(inputFile)
  
  # insert the YAML in after the first ---
  # I'm assuming all my RMDs have properly-formed YAML and that the first
  # occurence of --- starts the YAML. 
  # You could do proper validation if you wanted.
  yamlHeader <- grep('^---$', rmd)[1]
  # put the yaml in
  rmd <- append(rmd, yaml, after = yamlHeader)
  
  # write out to a temp file
  ofile <- file.path(tempdir(), basename(inputFile))
  writeLines(rmd, ofile)
  
  # render with rmarkdown.
  message(ofile)
  ofile <-
    rmarkdown::render(ofile,
                      encoding = encoding,
                      envir = new.env(),
                      output_dir = output_dir)
  
  # copy back to the current directory.
  file.copy(ofile, file.path(dirname(inputFile), basename(ofile)),
            overwrite = TRUE)
}
