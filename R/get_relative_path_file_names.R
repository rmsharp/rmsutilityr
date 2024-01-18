#' Creates file names using a common relative path structure
#' 
#' @returns A dataframe with columns `original_file` and `new_file`
#' 
#' @param files character vector of full path file names
#' @param new_path character vector of length one having the new file path
#' common to be prepended to the relative paths created. Default is ".".
#' @importFrom utils head
#' @importFrom stringi stri_split_fixed stri_c
#' @export
get_relative_path_file_names <- function(files, new_path = ".") {
    all_dirs <- list(length(files))
    for (i in seq_along(files)) {
      ## Need a function that get the vector of directories in a file name
      all_dirs[[i]] <-
        c(head(stri_split_fixed(str = files[i], pattern = "/")[[1]],-1))
    }
    path_len <- length(Reduce(intersect,all_dirs))
    new_dirs <- list(length(files))
    i <- 0
    for (dirs in all_dirs) {
      i <- i + 1
      if (length(dirs) == path_len)
        new_dirs[[i]] <- new_path
      else
        new_dirs[[i]] <- dirs[-(1:path_len)]
    }

    data.frame(original_file = files,
               new_file = vapply(seq_along(new_dirs), function(i) {
                 stri_c(stri_c(new_dirs[[i]], collapse = "/"),
                        basename(files[i]), sep = "/")
               },
               character(1)))
    
}
