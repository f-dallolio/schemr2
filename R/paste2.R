#' Variations of `paste`
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' @param .before a character vecror.
#' @param sep a character string to separate the terms. Not NA_character_.
#' @param collapse an optional character string to separate the results. Not NA_character_. When collapse is a string, the result is always a string (character of length 1).
#' @param recycle0 	logical indicating if zero-length character arguments should result in the zero-length chara
#'
#' @return A character vector of the concatenated values.
#'
#' @export
paste2 <- function(..., .before = NULL, sep = "",
                   collapse = NULL, recycle0 = FALSE){
  out <- paste(..., sep = sep, collapse = collapse, recycle0 = recycle0)
  paste(.before, out, sep = sep, collapse = collapse, recycle0 = recycle0)
}
