#' Returns even numbers from a set of numbers
#'
#' @param x vector of numbers (only integers should be used)
#' @export
is_even <- function(x) x %% 2 == 0
#' Returns odd numbers from a set of numbers
#'
#' @param x vector of numbers (only integers should be used)
#' @export
is_odd <- function(x) x %% 2 == 1
#' Returns dataframe with ordered locations of the matching braces.
#'
#' @param txt character vector of length one having 0 or more matching braces.
#' @import stringi
#' @examples
#' library(rmsutilityr)
#' match_braces("{123{456{78}9}10}")
#' @export
match_braces <- function(txt) {
  txt <- txt[1] # just in the case of having more than one element
  left <- stri_locate_all_regex(txt, "\\{")[[1]][ , 1]
  right <- stri_locate_all_regex(txt, "\\}")[[1]][ , 2]
  len <- length(left)
  braces <- data.frame(left = rep(0, len), right = rep(0, len))
  for (i in seq_along(right)) {
    for (j in rev(seq_along(left))) {
      if (left[j] < right[i] & left[j] != 0) {
        braces$left[i] <- left[j]
        braces$right[i] <- right[i]
        left[j] <- 0
        break
      }
    }
  }
  braces[order(braces$left), ]
}
