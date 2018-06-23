#' Returns a character vector made up of two vectors, where if both are is.na()
#' NA is returned, if one is.na() the other is returned and if neither are
#' is.na() then the first is returned.
#'
#' @param vec1 first character vector
#' @param vec2 second character vector
#' @export
combine_vectors <- function(vec1, vec2) {
  if (length(vec1) != length(vec2))
    stop("vectors must be the same length")
  combined <- character(length(vec1))
  both <- seq_along(vec1)[!is.na(vec1) & !is.na(vec2)]
  use_vec1 <- seq_along(vec1)[!is.na(vec1) & is.na(vec2)]
  use_vec2 <- seq_along(vec1)[is.na(vec1) & !is.na(vec2)]
  neither <- seq_along(vec1)[is.na(vec1) & is.na(vec2)]
  combined[c(both, use_vec1)] <- vec1[c(both, use_vec1)]
  combined[use_vec2] <- vec2[use_vec2]
  combined[neither] <- NA
  combined
}
